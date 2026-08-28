/**
 * @param {{ icons: Array<{ icon: { paths: string[], attrs: object[] }, properties: { name: string } }> }} selection
 * @returns {Map<string, { paths: string[], attrs: object[] }>}
 */
export function buildIconMap(selection) {
  const map = new Map();
  for (const icon of selection.icons) {
    const { name } = icon.properties;
    // Strip weight suffix to get base name (e.g. "gear-bold" → "gear")
    // Take only the primary name before any comma-separated aliases
    const primaryName = name.split(",")[0].trim();
    const baseName = primaryName.replace(/-(?:bold|fill|light|thin|regular|duotone)$/, "");
    map.set(baseName, { paths: icon.icon.paths, attrs: icon.icon.attrs });
  }
  return map;
}

/**
 * @param {string[]} paths
 * @param {object[]} attrs
 * @returns {string}
 */
export function makeSvgDataUrl(paths, attrs) {
  const pathEls = paths
    .map((d, i) => {
      const a = attrs[i] ?? {};
      const attrStr = Object.entries(a)
        .map(([k, v]) => `${k}="${v}"`)
        .join(" ");
      return attrStr ? `<path ${attrStr} d="${d}"/>` : `<path d="${d}"/>`;
    })
    .join("");
  return `data:image/svg+xml,${encodeURIComponent(`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1024 1024" fill="currentColor">${pathEls}</svg>`)}`;
}
