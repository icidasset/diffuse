import {
  ApiError,
  CMD,
  Signer,
  WebCryptoSigner,
  delegate,
  inlineInvocation,
} from "@tokono.ma/firkin";

import {
  adoptPasskeyPrfResult,
  createPasskey,
} from "~/components/transformer/output/refiner/passkey-encryption/passkey.js";
import {
  clientForSession,
  deleteSession,
  readSession,
  saveSession,
} from "./session.js";
import { forgetOutputKey, storePasskeyOutputKey } from "./output-key.js";

/**
 * @import { UcanSigner } from "@tokono.ma/firkin"
 * @import { Session } from "./session.js"
 */

/** Default endpoint for the local development instance. */
export const DEFAULT_API_BASE = "http://localhost:8787";

/**
 * The UCAN delegation granted to the browser session key. `/firkin` covers the
 * blob, registry, tag and channel commands. Firkin still requires the account
 * key directly for register/revoke/recover, so this is not an admin grant.
 */
export const SESSION_COMMAND = "/firkin";

/**
 * How long a session delegation lasts before the passkey is needed again.
 * One year; the session itself is revocable by the account key.
 */
export const SESSION_TTL_SECONDS = 365 * 24 * 60 * 60;

/**
 * Passkey namespace. This only scopes the stored credential ID; the PRF
 * derivation itself is based on the relying-party ID.
 */
export const PASSKEY_NAMESPACE = "firkin";

/**
 * Thrown when a Firkin instance rejects the account: its DID has to be
 * registered before the registry is readable, so the UI surfaces the DID for
 * copying instead of burying it in an error message.
 */
export class UnregisteredAccountError extends Error {
  /** @param {string} did */
  constructor(did) {
    super(
      "This account is not registered with the Firkin instance. Register its DID in the instance's admin console, then sign in.",
    );

    this.name = "UnregisteredAccountError";

    /** The DID that has to be registered. */
    this.did = did;
  }
}

/**
 * @typedef {Object} PasskeyMaterial
 * @property {Uint8Array} credentialId
 * @property {ArrayBuffer} prfFirst
 * @property {ArrayBuffer} prfSecond
 */

/**
 * Run the passkey flow and return its PRF material.
 *
 * @param {boolean} [create=false]
 * @returns {Promise<PasskeyMaterial>}
 */
async function passkeyMaterial(create = false) {
  const prf = create
    ? await createPasskey(PASSKEY_NAMESPACE)
    : await adoptPasskeyPrfResult(PASSKEY_NAMESPACE);

  if (!prf.supported) throw new Error(prf.reason);
  if (!prf.prfFirst) {
    throw new Error(
      "This authenticator did not return the PRF signing output",
    );
  }

  return /** @type {PasskeyMaterial} */ (prf);
}

/**
 * Turn the passkey's PRF "signing" output into the account's ed25519 signer.
 *
 * Prefers a non-extractable WebCrypto key so the seed never lives in JS
 * memory longer than the import. Falls back to the raw seed on browsers
 * without WebCrypto Ed25519.
 *
 * @param {ArrayBuffer} prfFirst
 * @returns {Promise<UcanSigner>}
 */
export async function accountSignerFromPrf(prfFirst) {
  const seed = new Uint8Array(prfFirst);

  if (seed.length !== 32) {
    throw new Error("Passkey PRF output was not 32 bytes");
  }

  try {
    return await WebCryptoSigner.fromSeedNonExtractable(seed);
  } catch {
    return await Signer.fromSeed(seed);
  }
}

/**
 * Derive the DID for an existing passkey (or create one). Purely local: no
 * request is made, so the DID can be registered before signing in.
 *
 * @param {{ create?: boolean }} [options]
 * @returns {Promise<string>}
 */
export async function passkeyDid(options = {}) {
  const prf = await passkeyMaterial(options.create ?? false);
  const signer = await accountSignerFromPrf(prf.prfFirst);
  return signer.did();
}

/**
 * Read the connected account, if the stored session is still valid.
 *
 * @returns {Promise<{ session: Session; did: string; apiBase: string } | undefined>}
 */
export async function readAccount() {
  const session = await readSession();
  if (!session) return undefined;

  return {
    session,
    did: session.did,
    apiBase: session.apiBase,
  };
}

/**
 * Mint a one-year `/firkin` delegation for a browser session key.
 *
 * A previously stored session key is reused when it belongs to the same
 * instance + account, but the delegation is always re-minted. This makes an
 * explicit passkey sign-in recover from a delegation that was revoked or
 * expired elsewhere.
 *
 * @param {string} apiBase
 * @param {string} did
 * @param {UcanSigner} accountSigner
 * @returns {Promise<Session>}
 */
async function createSession(apiBase, did, accountSigner) {
  const stored = await readSession();

  const sessionSigner =
    stored &&
      stored.apiBase === apiBase &&
      stored.did === did &&
      stored.privateKey instanceof CryptoKey
      ? WebCryptoSigner.fromPersisted(stored.privateKey, stored.publicKey)
      : await WebCryptoSigner.generateNonExtractable();

  const { proof, cid } = await delegate(accountSigner, {
    subject: did,
    audience: sessionSigner.did(),
    command: SESSION_COMMAND,
    expireIn: SESSION_TTL_SECONDS,
  });

  /** @type {Session} */
  const session = {
    apiBase,
    did,
    privateKey: /** @type {CryptoKey} */ (sessionSigner.privateKey),
    publicKey: sessionSigner.pubkeyBytes,
    proof,
    proofCid: cid,
    expiresAt: Date.now() + SESSION_TTL_SECONDS * 1000,
    // Start with inline proofs: another tab/worker may read this session
    // before the publish below completes.
    inlineProofs: true,
  };

  await saveSession(session);
  return session;
}

/**
 * Publish the session delegation to the server's delegation store so tokens
 * can reference it by CID. Idempotent, so it is safe to run on every restore.
 *
 * Falls back to inline proofs on servers that have no delegation store.
 *
 * @param {Session} session
 */
export async function publishSessionDelegation(session) {
  const sessionSigner = WebCryptoSigner.fromPersisted(
    session.privateKey,
    session.publicKey,
  );

  const rb = clientForSession({ ...session, inlineProofs: true });

  try {
    // Bootstrap token: the delegation is not stored yet, so its proof has to
    // travel inline for this one call. Publishing is its own capability.
    const token = await inlineInvocation(sessionSigner, {
      command: `${CMD.delegationPublish}/${session.proofCid}`,
      proofs: [{ proof: session.proof, cid: session.proofCid }],
      expireIn: 60,
    });

    await rb.publishDelegation(
      { proof: session.proof, cid: session.proofCid },
      { token },
    );
    session.inlineProofs = false;
  } catch (err) {
    console.warn(
      "Could not publish the Firkin session delegation; falling back to inline proofs:",
      err,
    );
    session.inlineProofs = true;
  }

  await saveSession(session);
}

/**
 * Sign in with a passkey, create/publish the session, and verify that the
 * account is registered.
 *
 * @param {{ apiBase?: string; create?: boolean }} [options]
 * @returns {Promise<{ apiBase: string; did: string; session: Session; prf: PasskeyMaterial; accountSigner: UcanSigner }>}
 */
export async function signInAccount(options = {}) {
  const apiBase = (options.apiBase?.trim() || DEFAULT_API_BASE)
    .replace(/\/+$/, "");

  const prf = await passkeyMaterial(options.create ?? false);
  const accountSigner = await accountSignerFromPrf(prf.prfFirst);
  const did = accountSigner.did();

  const session = await createSession(apiBase, did, accountSigner);
  await publishSessionDelegation(session);

  try {
    // A cheap authenticated read proves the account is registered.
    const rb = clientForSession(session);
    await rb.registryList();
  } catch (err) {
    if (
      err instanceof ApiError && (err.status === 401 || err.status === 403)
    ) {
      throw new UnregisteredAccountError(did);
    }
    throw err;
  }

  // Encryption is opt-in: a failure here must not stop the sign-in.
  try {
    await storePasskeyOutputKey(prf.prfSecond);
  } catch (err) {
    console.warn("Could not derive the output encryption key:", err);
  }

  return { apiBase, did, session, prf, accountSigner };
}

/** Forget the local account session and the derived output key. */
export async function signOutAccount() {
  await deleteSession();
  await forgetOutputKey();
}
