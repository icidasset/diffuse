import {
  BroadcastableDiffuseElement,
  defineElement,
  query,
} from "~/common/element.js";
import { computed, signal } from "~/common/signal.js";
import * as Collaboration from "~/common/collaboration/index.js";
import * as Playlist from "~/common/playlist.js";

/**
 * @import {Collaboration as CollaborationRecord, PlaylistItem} from "~/definitions/types.d.ts"
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import {CollaborationConfiguratorElement} from "@specs/components/configurator/collaboration/types.d.ts"
 * @import {RemotePlaylistGroup} from "~/common/collaboration/index.js"
 */

/**
 * Builds the cohesive view of playlists shared with collaborators.
 *
 * Unlike a transformer, this orchestrator does not alter the output's
 * `playlistItems` collection. It reads the local collaboration records from the
 * given output, fetches each collaborator's playlist items over AT Protocol,
 * and exposes a merged collection that a facet can render. Items contributed by
 * collaborators are scoped by their owner's DID and filtered out when saving.
 *
 * Collaborator items are fetched by the broadcasted leader only; other tabs
 * receive the merged state through the broadcast channel.
 *
 * The orchestrator is protocol-agnostic: it resolves a collaborator's `hint`
 * through a collaboration configurator (`dc-collaboration`). Append source
 * elements to that configurator — for example `dcs-atproto` — and point the
 * orchestrator at it with `collaboration-configurator-selector`.
 *
 * @extends {BroadcastableDiffuseElement}
 */
class CooperativePlaylistsOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/cooperative-playlists";

  static observedAttributes = ["depth"];

  /** @type {import("~/common/signal.d.ts").Signal<OutputElement | null>} */
  #output = signal(/** @type {OutputElement | null} */ (null));

  /** @type {import("~/common/signal.d.ts").Signal<CollaborationConfiguratorElement | null>} */
  #configurator = signal(
    /** @type {CollaborationConfiguratorElement | null} */ (null),
  );

  /** @type {import("~/common/signal.d.ts").Signal<RemotePlaylistGroup[]>} */
  #remote = signal(/** @type {RemotePlaylistGroup[]} */ ([]));

  /** @type {import("~/common/signal.d.ts").Signal<number>} */
  #depth = signal(1);

  /** @type {Map<string, Promise<RemotePlaylistGroup | null>>} */
  #cache = new Map();

  #generation = 0;

  /** @type {CollaborationRecord[] | undefined} */
  #lastCollaborations;

  // STATE

  /**
   * The local collaboration records.
   */
  collaborations = computed(() => {
    const output = this.#output.value;
    if (!output) return [];

    const col = output.collaborations.collection();
    return col.state === "loaded" ? col.data : [];
  });

  /**
   * Local playlist items merged with the items contributed by collaborators.
   */
  playlistItems = computed(() => {
    const output = this.#output.value;
    if (!output) return [];

    const col = output.playlistItems.collection();
    if (col.state !== "loaded") return [];

    return Collaboration.mergeItems(col.data, this.#remote.value);
  });

  /**
   * The merged playlist items grouped by playlist name, with ordered/unordered
   * state, sorted alphabetically.
   */
  playlists = computed(() => {
    return [...Playlist.gather(this.playlistItems()).values()]
      .sort((a, b) => a.name.localeCompare(b.name));
  });

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        getRemoteGroups: {
          strategy: "leaderOnly",
          fn: this.#remote.get,
        },
        setRemoteGroups: {
          strategy: "replicate",
          fn: this.#remote.set,
        },
        refresh: {
          strategy: "leaderOnly",
          fn: this.refresh,
        },
      });

      if (actions) {
        this.#remote.set = actions.setRemoteGroups;
        this.refresh = actions.refresh;

        actions.getRemoteGroups().then((groups) => {
          this.#remote.value = groups;
        });
      }
    }

    super.connectedCallback();

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {CollaborationConfiguratorElement} */
    const configurator = query(
      this,
      "collaboration-configurator-selector",
    );

    await Promise.all([
      customElements.whenDefined(output.localName),
      customElements.whenDefined(configurator.localName),
    ]);

    this.#output.value = output;
    this.#configurator.value = configurator;

    // Fetch collaborator items whenever the collaboration records change.
    this.effect(async () => {
      const configurator = this.#configurator.value;
      if (!configurator) return;

      const col = output.collaborations.collection();
      if (col.state !== "loaded") return;

      const depth = this.#depth.get();

      if ((await this.isLeader()) === false) return;

      if (col.data !== this.#lastCollaborations) {
        this.#lastCollaborations = col.data;
        this.#cache.clear();
      }

      return this.#load(configurator, col.data, depth);
    });
  }

  /**
   * @override
   * @param {string} name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(name, oldValue, newValue) {
    super.attributeChangedCallback(name, oldValue, newValue);

    if (name === "depth") this.#depth.set(parseDepth(newValue));
  }

  // ACTIONS

  /**
   * Re-fetch every collaborator's items, bypassing the cache.
   */
  refresh = async () => {
    const output = this.#output.value;
    const configurator = this.#configurator.value;
    if (!output || !configurator) return;

    await output.collaborations.reload();

    const col = output.collaborations.collection();
    if (col.state !== "loaded") return;

    this.#lastCollaborations = col.data;
    this.#cache.clear();
    await this.#load(configurator, col.data, this.#depth.get());
  };

  /**
   * Save local playlist items. Collaborator items are never written back to the
   * output.
   *
   * @param {PlaylistItem[]} items
   */
  save = async (items) => {
    const output = this.#output.value;
    if (!output) return;

    await output.playlistItems.save(
      items.filter((item) => !Collaboration.isRemoteItem(item)),
    );
  };

  // FETCHING

  /**
   * @param {CollaborationConfiguratorElement} configurator
   * @param {CollaborationRecord[]} collaborations
   * @param {number} depth
   */
  async #load(configurator, collaborations, depth) {
    const generation = ++this.#generation;

    /** @param {import("~/definitions/types.d.ts").Collaborator} collaborator */
    const isSupported = (collaborator) =>
      configurator.sourceFor(collaborator.hint) !== undefined;

    const graph = await Collaboration.expandCollaborators(collaborations, {
      depth,
      load: (collaborator) => {
        const source = configurator.sourceFor(collaborator.hint);
        return source
          ? source.loadCollaborations(collaborator.id)
          : Promise.resolve([]);
      },
      isSupported,
    });

    /** @type {Promise<RemotePlaylistGroup | null>[]} */
    const tasks = [];

    for (const [playlist, collaborators] of graph) {
      for (const collaborator of collaborators) {
        if (!isSupported(collaborator)) continue;
        tasks.push(this.#loadGroup(configurator, playlist, collaborator));
      }
    }

    const groups = (await Promise.all(tasks)).filter(
      (group) => group !== null,
    );

    if (generation !== this.#generation) return;
    this.#remote.value = groups;
  }

  /**
   * @param {CollaborationConfiguratorElement} configurator
   * @param {string} playlist
   * @param {import("~/definitions/types.d.ts").Collaborator} collaborator
   * @returns {Promise<RemotePlaylistGroup | null>}
   */
  #loadGroup(configurator, playlist, collaborator) {
    const source = configurator.sourceFor(collaborator.hint);
    if (!source) return Promise.resolve(null);

    const key = `${playlist}\0${collaborator.id}`;
    const cached = this.#cache.get(key);
    if (cached) return cached;

    const promise = source.loadPlaylistItems(collaborator.id, playlist)
      .then(({ owner, items }) => ({
        playlist,
        owner,
        label: collaborator.id,
        items,
      }))
      .catch((err) => {
        console.warn(
          `Could not load playlist "${playlist}" from ${collaborator.id}:`,
          err,
        );
        return null;
      });

    this.#cache.set(key, promise);
    return promise;
  }
}

/**
 * @param {string | null | undefined} value
 * @returns {number}
 */
function parseDepth(value) {
  const parsed = Number.parseInt(value ?? "", 10);
  if (Number.isNaN(parsed) || parsed < 1) return 1;
  return Math.min(parsed, 5);
}

export default CooperativePlaylistsOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = CooperativePlaylistsOrchestrator;
export const NAME = "do-cooperative-playlists";

defineElement(NAME, CLASS);
