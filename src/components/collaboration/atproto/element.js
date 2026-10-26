import { defineElement, DiffuseElement } from "~/common/element.js";
import { loadCollaborations, loadPlaylistItems } from "~/common/atproto.js";

/**
 * @import {Collaboration} from "~/definitions/types.d.ts"
 * @import {CollaborationSourceActions, CollaborationSourceProvider} from "@specs/components/collaboration/types.d.ts"
 */

export const PREFIX = /** @type {const} */ ("atproto");

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Collaboration source for AT Protocol identities.
 *
 * @implements {CollaborationSourceProvider}
 * @implements {CollaborationSourceActions}
 */
class ATProtoCollaborationSource extends DiffuseElement {
  static NAME = "diffuse/collaboration/atproto";

  PREFIX = PREFIX;

  /** @param {string} actor */
  async loadCollaborations(actor) {
    return /** @type {Collaboration[]} */ (await loadCollaborations(actor));
  }

  /**
   * @param {string} actor
   * @param {string} playlist
   */
  async loadPlaylistItems(actor, playlist) {
    const { identity, items } = await loadPlaylistItems(actor, playlist);
    return { owner: identity.did, items };
  }
}

export default ATProtoCollaborationSource;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ATProtoCollaborationSource;
export const NAME = "dcs-atproto";

defineElement(NAME, CLASS);
