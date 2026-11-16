/**
 * Best-effort change hints over a Firkin channel.
 *
 * The channel is a relay: it may drop, duplicate or reorder messages, and it
 * carries no data. A hint only says "collection X changed", after which the
 * reader re-reads the signed manifests. A missed hint costs latency, never
 * correctness — which is why the connection is entirely optional.
 */

/** Prefix of the channel name used for output hints. */
export const CHANNEL_PREFIX = "diffuse:output:";

/** Debounce for a burst of hints about the same collection. */
export const HINT_DEBOUNCE_MS = 100;

/**
 * Channel names also travel inside a UCAN command
 * (`/firkin/channel/join/<channel>`), so they are lowercased.
 *
 * @param {string | null | undefined} namespace
 */
export function channelName(namespace) {
  return `${CHANNEL_PREFIX}${namespace ?? "default"}`.toLowerCase();
}

/**
 * @typedef {{ collection: string; device?: string }} Hint
 */

/**
 * Parse a hint frame, rejecting anything that isn't one.
 *
 * @param {unknown} data
 * @returns {Hint | undefined}
 */
export function parseHint(data) {
  if (typeof data !== "string") return undefined;

  try {
    const value = JSON.parse(data);

    if (!value || value.v !== 1) return undefined;
    if (typeof value.collection !== "string" || value.collection.length === 0) {
      return undefined;
    }

    return {
      collection: value.collection,
      device: typeof value.device === "string" ? value.device : undefined,
    };
  } catch {
    return undefined;
  }
}

/**
 * Open a hint channel and keep it open, reconnecting while it's wanted.
 *
 * `url` is a function because Firkin issues a short-lived token per connection:
 * every (re)connect asks for a fresh URL.
 *
 * @param {Object} options
 * @param {string} options.device - Ignore our own hints.
 * @param {() => Promise<string | undefined>} options.url
 * @param {(collection: string) => void} options.onHint
 * @param {typeof WebSocket} [options.WebSocketImpl] - Injectable for tests.
 * @returns {{ send(collection: string): void; close(): void }}
 */
export function openHintChannel(
  { device, url, onHint, WebSocketImpl = WebSocket },
) {
  /** @type {WebSocket | undefined} */
  let socket;

  let closed = false;
  let retries = 0;
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  let reconnect;

  const connect = async () => {
    if (closed) return;

    const target = await url().catch(() => undefined);
    if (!target || closed) return;

    const opened = new WebSocketImpl(target);
    socket = opened;

    opened.addEventListener("open", () => {
      retries = 0;
    });

    opened.addEventListener("message", (event) => {
      // A socket can deliver a queued frame after close().
      if (closed) return;

      const hint = parseHint(/** @type {MessageEvent} */ (event).data);
      if (!hint) return;
      if (hint.device === device) return;

      onHint(hint.collection);
    });

    opened.addEventListener("close", () => {
      if (socket === opened) socket = undefined;
      if (closed) return;

      // Exponential backoff: the server may be down or the token stale.
      const delay = Math.min(30_000, 500 * 2 ** retries++);
      reconnect = setTimeout(() => void connect(), delay);
    });
  };

  void connect();

  return {
    /**
     * Publish a hint. Silently does nothing while disconnected — the data is
     * already committed, so a lost hint only delays other devices.
     *
     * @param {string} collection
     */
    send(collection) {
      if (!socket || socket.readyState !== 1) return;

      try {
        socket.send(JSON.stringify({ v: 1, collection, device }));
      } catch {
        // A closing socket is not worth reporting.
      }
    },

    close() {
      closed = true;
      clearTimeout(reconnect);
      socket?.close();
      socket = undefined;
    },
  };
}
