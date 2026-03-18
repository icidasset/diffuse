export function update() {
  const nav = document.getElementById("diffuse-nav");
  const btn = document.getElementById("nav-overflow-btn");
  const menu = document.getElementById("nav-overflow-menu");

  if (!nav || !btn || !menu) return;

  const items = /** @type {HTMLElement[]} */ ([...nav.children]);

  // Reset: show all items, hide button
  for (const item of items) item.style.display = "";
  btn.style.display = "none";

  // No overflow — nothing to do
  if (nav.scrollWidth <= nav.clientWidth) return;

  // Show button (nav shrinks to accommodate it via flex)
  btn.style.display = "";

  // Hide items from right until nav content fits
  const hidden = [];
  for (let i = items.length - 1; i >= 0; i--) {
    if (nav.scrollWidth <= nav.clientWidth) break;
    items[i].style.display = "none";
    hidden.unshift(items[i]);
  }

  // Populate dropdown with clones (stripped of button styling)
  menu.innerHTML = "";
  for (const el of hidden) {
    if (el.classList.contains("divider")) continue;

    const clone = /** @type {HTMLElement} */ (el.cloneNode(true));
    clone.style.display = "";
    clone.classList.remove(
      "button",
      "button--transparent",
      "button--border",
      "button--bg-twist-2",
    );

    menu.appendChild(clone);
  }
}

export function watchResize() {
  const nav = document.getElementById("diffuse-nav");
  if (nav) new ResizeObserver(update).observe(nav);
}
