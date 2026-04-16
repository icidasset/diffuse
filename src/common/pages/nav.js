/**
 * Updates the active state of nav links based on the current URL.
 * Required because the nav now lives in <header> and is not replaced by PPR.
 */
export function updateActiveLinks() {
  const nav = document.getElementById("diffuse-nav");
  const menu = document.getElementById("nav-overflow-menu");

  for (const container of [nav, menu]) {
    if (!container) continue;
    for (const link of container.querySelectorAll("a[href]")) {
      const a = /** @type {HTMLAnchorElement} */ (link);
      const isActive = new URL(a.href).pathname === location.pathname;
      a.classList.toggle("button--transparent", !isActive);
    }
  }
}

export function update() {
  const nav = document.getElementById("diffuse-nav");
  const btn = document.getElementById("nav-overflow-btn");
  const menu = document.getElementById("nav-overflow-menu");

  if (!nav || !btn || !menu) return;

  const items = /** @type {HTMLElement[]} */ ([...nav.children]);

  // Reset: show all items, hide button, restore default icon
  for (const item of items) item.style.display = "";
  btn.style.display = "none";
  const span = btn.querySelector("span");
  if (span) span.innerHTML = `<i class="ph-fill ph-dots-three-outline"></i>`;

  // Available width is the nav-container's width (nav may be content-sized in column layouts)
  const availableWidth = (nav.parentElement ?? nav).clientWidth;

  // Measure total content width directly — scrollWidth is unreliable with
  // justify-content: flex-end because overflow spills left (negative direction)
  // and isn't reflected in scrollWidth.
  const gap = parseFloat(getComputedStyle(nav).columnGap) || 0;
  const contentWidth = () => {
    const visible = items.filter((el) => el.style.display !== "none");
    return visible.reduce((acc, el) => acc + el.offsetWidth, 0) +
      gap * Math.max(0, visible.length - 1);
  };

  // No overflow — nothing to do
  if (contentWidth() <= availableWidth) return;

  // Show button (takes up space; subtract its width + container gap from available)
  btn.style.display = "";
  const containerGap = parseFloat(getComputedStyle(nav.parentElement ?? nav).columnGap) || 0;

  // Hide items from right until content fits
  const hidden = [];
  for (let i = items.length - 1; i >= 0; i--) {
    if (contentWidth() <= availableWidth - btn.offsetWidth - containerGap) break;
    items[i].style.display = "none";
    hidden.unshift(items[i]);
  }

  // Update button label: show "Menu" when all items are hidden
  const allHidden = hidden.length === items.length;
  if (span) {
    span.innerHTML = allHidden
      ? `<i class="ph-bold ph-list"></i> Menu`
      : `<i class="ph-fill ph-dots-three-outline"></i>`;
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

let _observer = /** @type {ResizeObserver | undefined} */ (undefined);

export function watchResize() {
  const nav = document.getElementById("diffuse-nav");
  if (!nav) return;
  _observer?.disconnect();
  _observer = new ResizeObserver(update);
  // Observe the container — in column layouts the nav is content-sized and
  // won't resize, but the container does as the header width changes.
  _observer.observe(nav.parentElement ?? nav);
}
