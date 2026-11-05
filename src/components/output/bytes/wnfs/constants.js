/**
 * IndexedDB prefixes for this output.
 *
 * The session (delegated ed25519 key + proof) is stored separately from the
 * WNFS storage pointer (root CID + imported RSA-OAEP private key) so a session
 * refresh never has to re-derive the private key.
 */
export const IDB_PREFIX = "diffuse/output/bytes/wnfs";
/** IndexedDB key for the delegated session key + proof. */
export const IDB_SESSION = `${IDB_PREFIX}/session`;
/** IndexedDB key for the WNFS root pointer + imported exchange private key. */
export const IDB_STORAGE = `${IDB_PREFIX}/storage`;
/** Root CID that the local collection cache corresponds to. */
export const IDB_LOCAL_ROOT = `${IDB_PREFIX}/local-root`;
/** Prefix for cached immutable WNFS blocks. */
export const IDB_BLOCK_PREFIX = `${IDB_PREFIX}/block`;
/**
 * Prefix for the last-known-good plaintext collection cache.
 *
 * This is not a sync layer: it only keeps the most recent successfully read
 * or written collection so the app can still render its facets/preludes when
 * the WNFS session is unavailable (expired passkey, offline, …).
 */
export const IDB_LOCAL_PREFIX = `${IDB_PREFIX}/local`;

/**
 * Registry names in the Firkin account registry:
 *
 * - `diffuse.storage.root` → the WNFS data root (CID of the root tree).
 * - `diffuse.storage.private-key` → a blob containing the WNFS exchange key
 *   private JWK, encrypted with the passkey-derived XChaCha20-Poly1305 key.
 *
 * Registry names cannot contain `/`, see `rb_core::registry::validate_name`.
 */
export const REGISTRY_ROOT = "diffuse.storage.root";
/** Registry name for the encrypted WNFS exchange private key blob. */
export const REGISTRY_PRIVATE_KEY = "diffuse.storage.private-key";

/**
 * Debounce for coalescing block uploads from rapid saves into one
 * `/dag/push` request.
 */
export const UPLOAD_DEBOUNCE_MS = 2_500;

/**
 * Maximum time to wait for the initial WNFS restore before using the local
 * cache. Keeps the dashboard from spinning forever if a restore request hangs.
 */
export const RESTORE_TIMEOUT_MS = 10_000;

/**
 * The UCAN delegation granted to the browser session key. `/firkin` covers every
 * Firkin command (blobs, registry and account recovery).
 */
export const SESSION_COMMAND = "/firkin";

/**
 * How long a session delegation lasts before the passkey is needed again.
 * The session itself is revocable by the account key.
 */
export const SESSION_TTL_SECONDS = 30 * 24 * 60 * 60;

/**
 * Default endpoint for the local development instance.
 */
export const DEFAULT_API_BASE = "http://localhost:8787";

/**
 * Directory inside the WNFS private root where the encoded output collections
 * live. Each collection is a separate file.
 */
export const WNFS_OUTPUT_DIR = "diffuse-output";

/**
 * Passkey namespace (the PRF salt strings are shared with the existing
 * Diffuse passkey helper, so this only scopes the stored credential ID).
 */
export const PASSKEY_NAMESPACE = "wnfs-firkin";

/**
 * HKDF salt/info for the symmetric key that protects the WNFS private key
 * blob stored in the Firkin registry.
 */
export const KEY_DERIVATION_SALT = "wnfs-firkin-private-key-salt";
/** HKDF info string for the private-key encryption key. */
export const KEY_DERIVATION_INFO = "wnfs-firkin-private-key";
