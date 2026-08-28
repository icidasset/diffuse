/**
 * @param {MouseEvent} event
 */
export function highlightTableEntry(event) {
  if (event.target instanceof HTMLElement === false) return;

  const tr = event.target.tagName === "TR"
    ? event.target
    : event.target.closest("tr");

  if (!tr) return;
  if (tr.closest("thead")) return;

  tr.parentElement?.querySelector("tr.highlighted")?.classList.remove(
    "highlighted",
  );

  tr.classList.add("highlighted");
}
