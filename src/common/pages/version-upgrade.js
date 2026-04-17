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
 * @param {{ version: string, cid: string }[]} artifacts
 * @returns {{ version: string, cid: string } | null}
 */
function getLatestArtifact(artifacts) {
  return Object.values(artifacts).reduce(
    /** @param {{ version: string, cid: string } | null} max */
    (max, artifact) => {
      if (!canParse(artifact.version)) return max;
      if (!max) return artifact;
      return greaterThan(parseSemver(artifact.version), parseSemver(max.version))
        ? artifact
        : max;
    },
    /** @type {{ version: string, cid: string } | null} */ (null),
  );
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

    return;
  }

  const versionOrCid =
    document.location.pathname.slice(1).split("/")[0]?.toLowerCase() ?? "";
  const usesCid = versionOrCid.startsWith("bafy");

  const { default: artifacts } = await import(
    `${document.location.origin}/artifacts.json`,
    { with: { type: "json" } }
  ).catch(() => ({ default: {} }));

  const lastArtifact = getLatestArtifact(artifacts);
  const isLatest = checkIsLatest(versionOrCid, lastArtifact);

  document.querySelectorAll("#status").forEach((status) => {
    removeLoadingAnimation(status);
    updateUpgradeLink(status, { usesCid, isLatest });
  });
}
