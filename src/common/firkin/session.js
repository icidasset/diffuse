import * as IDB from "idb-keyval";
import {
  ApiError,
  delegationTokenSource,
  Firkin,
  inlineInvocation,
  WebCryptoSigner,
} from "@tokono.ma/firkin";

/**
 * @import { BlockCache, TokenSource } from "@tokono.ma/firkin"
 */

/** IndexedDB key for the delegated Firkin account session. */
export const IDB_SESSION = "diffuse/firkin/session";

/**
 * A stable local-storage scope for one server + account.
 *
 * Output state must never be reused across accounts, even when the output
 * method (and therefore its namespace) is the same.
 *
 * @param {{ apiBase: string; did: string }} session
 */
export function sessionScope(session) {
  return `${encodeURIComponent(session.apiBase)}/${session.did}`;
}

/**
 * Whether a Firkin error means the session is no longer accepted.
 *
 * Only explicit authorization failures count: a 404, a 5xx or a network error
 * is not an auth problem and must not make a remote output look unusable.
 *
 * @param {unknown} err
 */
export function isUnauthorized(err) {
  return err instanceof ApiError && (err.status === 401 || err.status === 403);
}

/**
 * The Firkin account session shared by the storage output and the Firkin
 * input/uploader components.
 *
 * The session is a delegated ed25519 key; the account (passkey) is only
 * needed to mint a fresh delegation.
 *
 * @typedef {Object} Session
 * @property {string} apiBase
 * @property {string} did
 * @property {CryptoKey} privateKey
 * @property {Uint8Array} publicKey
 * @property {Uint8Array} proof
 * @property {string} proofCid
 * @property {number} expiresAt - Unix ms.
 * @property {boolean} [inlineProofs] - The server rejected delegation publishing
 *   (or it failed), so tokens carry their proof inline instead of by CID.
 */

/**
 * Read the persisted session, ignoring expired or malformed records.
 *
 * @returns {Promise<Session | undefined>}
 */
export async function readSession() {
  /** @type {Session | undefined} */
  const session = await IDB.get(IDB_SESSION);
  if (!session) return undefined;
  if (session.expiresAt <= Date.now()) return undefined;
  if (!(session.privateKey instanceof CryptoKey)) return undefined;

  return session;
}

/**
 * @param {Session} session
 */
export async function saveSession(session) {
  await IDB.set(IDB_SESSION, session);
}

export async function deleteSession() {
  await IDB.del(IDB_SESSION);
}

/**
 * Build a Firkin client for a delegated session.
 *
 * Reference the delegation by CID when it has been published (the SDK's
 * default; smaller tokens). Servers without a delegation store get the
 * bootstrap shape with the proof inline.
 *
 * @param {Session} session
 * @param {{ cache?: BlockCache }} [options]
 * @returns {Firkin}
 */
export function clientForSession(session, options = {}) {
  const sessionSigner = WebCryptoSigner.fromPersisted(
    session.privateKey,
    session.publicKey,
  );

  /** @type {TokenSource} */
  const tokenSource = session.inlineProofs
    ? async (command, tokenOptions) =>
      await inlineInvocation(sessionSigner, {
        command,
        proofs: [{ proof: session.proof, cid: session.proofCid }],
        expireIn: tokenOptions?.expireIn ?? 600,
      })
    : delegationTokenSource(
      sessionSigner,
      { cid: session.proofCid },
      { subject: session.did },
    );

  return new Firkin(
    session.apiBase,
    { tokenSource },
    { cache: options.cache },
  );
}

/** @type {{ key: string; session: Session; rb: Firkin } | undefined} */
let cached;

/**
 * The shared session + client for components that do not own the sign-in.
 *
 * Cached per worker and invalidated when the persisted session changes, so a
 * sign-in or delegation refresh in the output is picked up on the next call.
 *
 * @param {{ cache?: BlockCache }} [options]
 * @returns {Promise<{ session: Session; rb: Firkin } | undefined>}
 */
export async function getSessionClient(options = {}) {
  const session = await readSession();
  if (!session) {
    cached = undefined;
    return undefined;
  }

  const key = [
    session.apiBase,
    session.did,
    session.proofCid,
    session.expiresAt,
    session.inlineProofs ? "inline" : "published",
  ].join("\0");

  if (cached?.key !== key) {
    cached = {
      key,
      session,
      rb: clientForSession(session, options),
    };
  }

  return { session: cached.session, rb: cached.rb };
}
