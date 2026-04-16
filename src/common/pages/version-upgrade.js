import { canParse, greaterThan, parse as parseSemver } from "@std/semver";

// Version upgrade (only works with `diffuse-artifacts` deployments)
if (document.location.hostname.endsWith("diffuse.sh")) {
  document.querySelectorAll("#status").forEach(async (status) => {
    const versionOrCid =
      document.location.pathname.slice(1).split("/")[0]?.toLowerCase() ?? "";
    const usesCid = versionOrCid.startsWith("bafy");
    const { default: artifacts } = await import(
      `${document.location.origin}/artifacts.json`,
      { with: { type: "json" } }
    ).catch(() => ({ default: {} }));

    // Latest by semver (ignore non-semver versions)
    const lastArtifact = Object.values(artifacts).reduce((max, artifact) => {
      if (!canParse(artifact.version)) return max;
      if (!max) return artifact;
      return greaterThan(
          parseSemver(artifact.version),
          parseSemver(max.version),
        )
        ? artifact
        : max;
    }, null);

    // Check if using latest
    const isLatest = !lastArtifact
      ? true
      : usesCid
      ? versionOrCid === lastArtifact.cid
      : versionOrCid === lastArtifact.version;

    // Remove loading animation
    status.querySelectorAll(".ph-spinner").forEach((icon) => {
      icon.parentElement?.classList.add("hidden");

      setTimeout(() => {
        icon.parentElement?.classList.remove("animate-spin");
        icon.classList.remove("ph-spinner");
        icon.classList.add("ph-arrow-fat-lines-up");
      }, 500);
    });

    // If using CID, append `/hash/` to href
    status.querySelectorAll(`[href="/latest/"]`).forEach((a) => {
      if (usesCid) a.setAttribute("href", "/latest/hash/");
      if (!isLatest) {
        setTimeout(() => {
          a.classList.remove("hidden");
        }, 750);
      }
    });
  });
} else {
  document.querySelectorAll("#status a").forEach((el) => {
    el.classList.add("hidden");
  });
}
