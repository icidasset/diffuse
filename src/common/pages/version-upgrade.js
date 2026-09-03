import {
  canParse,
  greaterThan,
  parse as parseSemver,
  satisfies,
  tryParseRange,
} from "@std/semver";

/**
 * Given the current URL segment and the latest known artifact, returns whether
 * the user is already on the latest version.
 *
 * @param {string} versionOrCid - The first path segment of the current URL
 * @param {{ cid: string, version: string } | null} lastArtifact - The latest artifact
 * @returns {boolean}
 *
 * @example No artifact means always latest
 * ```js
 * import { checkIsLatest } from "~/common/pages/version-upgrade.js";
 *
 * if (!checkIsLatest("4.0.0", null)) throw new Error("no artifact should be latest");
 * ```
 *
 * @example CID comparison
 * ```js
 * import { checkIsLatest } from "~/common/pages/version-upgrade.js";
 *
 * const artifact = { cid: "bafyabc", version: "4.0.0" };
 * if (!checkIsLatest("bafyabc", artifact)) throw new Error("matching CID should be latest");
 * if (checkIsLatest("bafyxyz", artifact)) throw new Error("different CID should not be latest");
 * ```
 *
 * @example Exact version comparison
 * ```js
 * import { checkIsLatest } from "~/common/pages/version-upgrade.js";
 *
 * const artifact = { cid: "bafyabc", version: "4.0.0" };
 * if (!checkIsLatest("4.0.0", artifact)) throw new Error("matching version should be latest");
 * if (checkIsLatest("3.9.0", artifact)) throw new Error("older version should not be latest");
 * ```
 *
 * @example Version range (e.g. 4.0.x)
 * ```js
 * import { checkIsLatest } from "~/common/pages/version-upgrade.js";
 *
 * const artifact = { cid: "bafyabc", version: "4.0.5" };
 * if (!checkIsLatest("4.0.x", artifact)) throw new Error("latest within range should be latest");
 * if (checkIsLatest("3.x", artifact)) throw new Error("latest outside range should not be latest");
 * ```
 *
 * @example Caret and tilde ranges
 * ```js
 * import { checkIsLatest } from "~/common/pages/version-upgrade.js";
 *
 * const artifact = { cid: "bafyabc", version: "4.1.0" };
 * if (!checkIsLatest("^4.0.1", artifact)) throw new Error("^4.0.1 should match 4.1.0");
 * if (checkIsLatest("~4.0.1", artifact)) throw new Error("~4.0.1 should not match 4.1.0");
 * ```
 *
 * @example Partial versions are filled in with zeros (>=4 is equivalent to >=4.0.0)
 * ```js
 * import { checkIsLatest } from "~/common/pages/version-upgrade.js";
 *
 * const artifact = { cid: "bafyabc", version: "4.1.0" };
 * if (!checkIsLatest(">=4", artifact)) throw new Error(">=4 should match 4.1.0");
 * if (checkIsLatest(">=5", artifact)) throw new Error(">=5 should not match 4.1.0");
 * ```
 *
 * @example Non-semver, non-range slugs are always latest
 * ```js
 * import { checkIsLatest } from "~/common/pages/version-upgrade.js";
 *
 * const artifact = { cid: "bafyabc", version: "4.0.0" };
 * if (!checkIsLatest("some-branch", artifact)) throw new Error("non-semver slug should be latest");
 * ```
 */
export function checkIsLatest(versionOrCid, lastArtifact) {
  if (!lastArtifact) return true;
  const usesCid = versionOrCid.startsWith("bafy");
  if (usesCid) return versionOrCid === lastArtifact.cid;
  if (canParse(versionOrCid)) return versionOrCid === lastArtifact.version;
  const versionRange = tryParseRange(versionOrCid);
  if (versionRange) {
    return satisfies(parseSemver(lastArtifact.version), versionRange);
  }
  return true;
}

/**
 * Coerce a version string that may contain `x` wildcards (e.g. `4.x-nightly`)
 * into a parseable semver string (e.g. `4.99.0-nightly`). Returns the original
 * string if it's already parseable, or `null` if coercion fails.
 *
 * @param {string} version
 * @returns {string | null}
 */
function tryCoerceVersion(version) {
  if (canParse(version)) return version;
  const coerced = version.replace(/[xX]/g, "99");
  if (canParse(coerced)) return coerced;
  // Fill in missing patch segment (e.g., "4.99-nightly" -> "4.99.0-nightly")
  const withPatch = coerced.replace(
    /^(\d+\.\d+)([+-]|$)/,
    "$1.0$2",
  );
  if (canParse(withPatch)) return withPatch;
  return null;
}

/**
 * Whether a coerced version string refers to a rolling nightly build rather
 * than a tagged release. Nightlies are never candidates for the `latest` alias.
 *
 * @param {string} comparable - A coerced, parseable semver string
 * @returns {boolean}
 */
function isNightly(comparable) {
  return parseSemver(comparable).prerelease?.includes("nightly") ?? false;
}

/**
 * @param {Record<string, { version: string, cid: string }>} artifacts
 * @param {{ includePrerelease?: boolean, excludeNightlies?: boolean }} [options]
 * @returns {{ version: string, cid: string } | null}
 *
 * @example Returns null for an empty artifact list
 * ```js
 * import { getLatestArtifact } from "~/common/pages/version-upgrade.js";
 *
 * if (getLatestArtifact({}) !== null) throw new Error("empty artifacts should return null");
 * ```
 *
 * @example Returns the highest semver artifact
 * ```js
 * import { getLatestArtifact } from "~/common/pages/version-upgrade.js";
 *
 * const artifacts = {
 *   a: { cid: "a", version: "4.0.0" },
 *   b: { cid: "b", version: "4.1.0" },
 *   c: { cid: "c", version: "3.9.0" },
 * };
 * if (getLatestArtifact(artifacts)?.cid !== "b") throw new Error("should return highest version");
 * ```
 *
 * @example Ignores non-semver versions
 * ```js
 * import { getLatestArtifact } from "~/common/pages/version-upgrade.js";
 *
 * const artifacts = {
 *   a: { cid: "a", version: "4.0.0" },
 *   b: { cid: "b", version: "some-branch" },
 * };
 * if (getLatestArtifact(artifacts)?.cid !== "a") throw new Error("should ignore non-semver versions");
 * ```
 *
 * @example Coerces x-wildcard prerelease (4.x-nightly beats 3.5.0)
 * ```js
 * import { getLatestArtifact } from "~/common/pages/version-upgrade.js";
 *
 * const artifacts = {
 *   a: { cid: "a", version: "3.5.0" },
 *   b: { cid: "b", version: "4.x-nightly" },
 * };
 * if (getLatestArtifact(artifacts)?.cid !== "b") {
 *   throw new Error("4.x-nightly should be latest");
 * }
 * ```
 *
 * @example Coerced x-wildcard with prerelease beats higher stable of same major
 * ```js
 * import { getLatestArtifact } from "~/common/pages/version-upgrade.js";
 *
 * const artifacts = {
 *   a: { cid: "a", version: "4.5.1" },
 *   b: { cid: "b", version: "4.x-nightly" },
 * };
 * if (getLatestArtifact(artifacts, { includePrerelease: true })?.cid !== "b") {
 *   throw new Error("4.x-nightly should beat 4.5.1 when includePrerelease is true");
 * }
 * ```
 *
 * @example Excludes prerelease artifacts when includePrerelease is false
 * ```js
 * import { getLatestArtifact } from "~/common/pages/version-upgrade.js";
 *
 * const artifacts = {
 *   a: { cid: "a", version: "4.1.0" },
 *   b: { cid: "b", version: "4.2.0-nightly.1" },
 * };
 * if (getLatestArtifact(artifacts, { includePrerelease: false })?.cid !== "a") {
 *   throw new Error("should exclude prerelease artifacts");
 * }
 * if (getLatestArtifact(artifacts, { includePrerelease: true })?.cid !== "b") {
 *   throw new Error("should include prerelease artifacts when opted in");
 * }
 * ```
 */

export function getLatestArtifact(
  artifacts,
  { includePrerelease = true, excludeNightlies = false } = {},
) {
  return Object.values(artifacts).reduce(
    /** @param {{ version: string, cid: string } | null} max */
    (max, artifact) => {
      const comparable = tryCoerceVersion(artifact.version);
      if (!comparable) return max;
      if (
        !includePrerelease && parseSemver(comparable).prerelease?.length
      ) return max;
      if (excludeNightlies && isNightly(comparable)) return max;
      if (!max) return artifact;
      return greaterThan(
          parseSemver(comparable),
          parseSemver(tryCoerceVersion(max.version) ?? max.version),
        )
        ? artifact
        : max;
    },
    /** @type {{ version: string, cid: string } | null} */ (null),
  );
}

/**
 * Resolve the version label to display for the current URL segment.
 *
 * @param {string} versionOrCid - The first path segment of the current URL
 * @param {Record<string, { version: string, cid: string }>} artifacts
 * @returns {string | null} The version to display, or `null` to show nothing
 *
 * @example Empty URL segment (root) shows nothing
 * ```js
 * import { getVersionLabel } from "~/common/pages/version-upgrade.js";
 *
 * if (getVersionLabel("", {}) !== null) throw new Error("no version at root");
 * ```
 *
 * @example Semver slugs are shown as-is
 * ```js
 * import { getVersionLabel } from "~/common/pages/version-upgrade.js";
 *
 * if (getVersionLabel("4.0.0", {}) !== "4.0.0") throw new Error("semver passes through");
 * ```
 *
 * @example Non-semver slugs (e.g. nightlies) are shown as-is
 * ```js
 * import { getVersionLabel } from "~/common/pages/version-upgrade.js";
 *
 * if (getVersionLabel("4.x-nightly", {}) !== "4.x-nightly") throw new Error("slug passes through");
 * ```
 *
 * @example `latest` resolves to the newest released version (nightlies excluded)
 * ```js
 * import { getVersionLabel } from "~/common/pages/version-upgrade.js";
 *
 * const artifacts = {
 *   a: { version: "4.0.0", cid: "bafya" },
 *   b: { version: "4.1.0", cid: "bafyb" },
 * };
 * if (getVersionLabel("latest", artifacts) !== "4.1.0") throw new Error("latest resolves");
 * ```
 *
 * @example A prerelease release (alpha) beats a nightly build
 * ```js
 * import { getVersionLabel } from "~/common/pages/version-upgrade.js";
 *
 * const artifacts = {
 *   a: { version: "4.0.0-alpha.1", cid: "bafya" },
 *   b: { version: "4.x-nightly", cid: "bafyb" },
 * };
 * if (getVersionLabel("latest", artifacts) !== "4.0.0-alpha.1") {
 *   throw new Error("latest should resolve to the most stable release, not a nightly");
 * }
 * ```
 *
 * @example `latest` falls back to the literal segment when no artifacts are known
 * ```js
 * import { getVersionLabel } from "~/common/pages/version-upgrade.js";
 *
 * if (getVersionLabel("latest", {}) !== "latest") throw new Error("latest fallback");
 * ```
 *
 * @example A CID resolves to its version when present in the artifacts
 * ```js
 * import { getVersionLabel } from "~/common/pages/version-upgrade.js";
 *
 * const artifacts = {
 *   a: { version: "4.0.0", cid: "bafya" },
 * };
 * if (getVersionLabel("bafya", artifacts) !== "4.0.0") throw new Error("CID resolves");
 * ```
 *
 * @example An unknown CID shows nothing
 * ```js
 * import { getVersionLabel } from "~/common/pages/version-upgrade.js";
 *
 * if (getVersionLabel("bafyunknown", {}) !== null) throw new Error("unknown CID hidden");
 * ```
 */
export function getVersionLabel(versionOrCid, artifacts) {
  if (!versionOrCid) return null;
  if (versionOrCid === "latest") {
    // The `latest` alias points at the most stable released artifact: never a
    // rolling nightly, but alpha/beta release candidates still qualify.
    return getLatestArtifact(artifacts, { excludeNightlies: true })?.version
      ?? "latest";
  }
  if (versionOrCid.startsWith("bafy")) {
    const artifact = Object.values(artifacts).find(
      ({ cid }) => cid === versionOrCid,
    );
    return artifact?.version ?? null;
  }
  return versionOrCid;
}

/** @param {Element} status */
function removeLoadingAnimation(status) {
  status.querySelectorAll(".ph-spinner").forEach((icon) => {
    icon.parentElement?.classList.add("hidden");

    setTimeout(() => {
      icon.parentElement?.classList.remove("animate-spin");
      icon.classList.remove("ph-spinner");
      icon.classList.add("ph-arrow-fat-lines-up");
    }, 500);
  });
}

/**
 * @param {Element} status
 * @param {{ usesCid: boolean, isLatest: boolean }} options
 */
function updateUpgradeLink(status, { usesCid, isLatest }) {
  status.querySelectorAll(`[href="/latest/"]`).forEach((a) => {
    if (usesCid) a.setAttribute("href", "/latest/hash/");
    if (!isLatest) setTimeout(() => a.classList.remove("hidden"), 750);
  });
}

/**
 * Setup version upgrade (only works with `diffuse-artifacts` deployments)
 */
export async function versionUpgrade() {
  const isDiffuseDomain = document.location.hostname.endsWith("diffuse.sh");

  if (!isDiffuseDomain) {
    document.querySelectorAll("#status a").forEach((el) => {
      el.classList.add("hidden");
    });

    document.querySelectorAll("#version").forEach((version) => {
      version.textContent = `v${version.getAttribute("data-fallback")}`
    });

    return;
  }

  const versionOrCid =
    document.location.pathname.slice(1).split("/")[0]?.toLowerCase() ?? "";
  const usesCid = versionOrCid.startsWith("bafy");

  const { default: artifacts } = await import(
    `${document.location.origin}/artifacts.json`,
    { with: { type: "json" } }
  ).catch(() => ({ default: {} }));

  const currentIsStable = canParse(versionOrCid) &&
    !parseSemver(versionOrCid).prerelease?.length;
  const lastArtifact = getLatestArtifact(artifacts, {
    includePrerelease: !currentIsStable,
    // The `latest` alias points at the most stable released artifact, never a
    // rolling nightly. Other non-semver slugs (e.g. `4.x-nightly`) keep
    // considering nightlies.
    excludeNightlies: versionOrCid === "latest",
  });
  const isLatest = checkIsLatest(versionOrCid, lastArtifact);

  document.querySelectorAll("#status").forEach((status) => {
    removeLoadingAnimation(status);
    updateUpgradeLink(status, { usesCid, isLatest });
  });

  const versionLabel = getVersionLabel(versionOrCid, artifacts);
  document.querySelectorAll("#version").forEach((version) => {
    if (versionLabel) version.textContent = `v${versionLabel}`;
  });
}
