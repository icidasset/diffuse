import * as Build from "./build.js";
import * as Dashboard from "./dashboard.js";
import * as Grid from "./grid.js";
import * as Guide from "./guide.js";
import * as Nav from "./nav.js";

/** Base pathname of the app (e.g. "/" at root, "/diffuse/" in a subdirectory). */
const BASE_PATHNAME = new URL(document.baseURI).pathname;

/**
 * Strips the app's base path prefix from an absolute pathname,
 * returning a root-relative path like "/build".
 *
 * @param {string} pathname
 */
function relativePathname(pathname) {
  const stripped = pathname.replace(/\/$/, "");
  const base = BASE_PATHNAME.replace(/\/$/, "");
  return base.length > 0 && stripped.startsWith(base)
    ? stripped.slice(base.length)
    : stripped;
}

/**
 * @param {URL} url
 */
async function initJsBasedOnPage(url) {
  const path = relativePathname(url.pathname);

  Nav.update();
  Nav.watchResize();

  Grid.setupFilter();
  Grid.insertToggleButtons();
  await Grid.monitorToggleButtonStates();

  switch (path) {
    case "/build":
      Build.renderEditor();
      Build.handleBuildFormSubmit();
      Build.listenForExamplesEdit();
      await Build.editFacetFromURL();
      break;
    case "/dashboard":
      await Dashboard.renderList();
      break;
    case "/guide":
      Guide.setupSampleButton();
      break;
    default:
      break;
  }
}

initJsBasedOnPage(new URL(location.href));

// Partial page updates for kitchen navigation using the Navigation API.
// Intercepts nav link clicks, fetches the new page, and swaps <main> content
// instead of doing a full page load.

if ("navigation" in globalThis) {
  /** @type {any} */ (globalThis).navigation.addEventListener(
    "navigate",
    navigateHandler,
  );
}

/** @param {any} event */
function navigateHandler(event) {
  if (!event.canIntercept) return;

  const url = new URL(event.destination.url);
  if (url.origin !== location.origin) return;

  // Only intercept paths one level deep
  const relative = relativePathname(url.pathname);
  const parts = relative.split("/").filter(Boolean);
  if (parts.length === 0) return;
  if (parts.length > 2) return;

  // Skip the loader page
  if (parts[0] === "l") return;
  if (parts.includes("chronicle")) return;

  event.intercept({
    scroll: "manual",
    async handler() {
      let html;

      try {
        const response = await fetch(url);
        if (!response.ok) throw new Error(`${response.status}`);
        html = await response.text();
      } catch {
        location.href = url.href;
        return;
      }

      const parser = new DOMParser();
      const doc = parser.parseFromString(html, "text/html");

      const newMain = doc.querySelector("main");
      const currentMain = document.querySelector("main");

      if (!newMain || !currentMain) {
        location.href = url.href;
        return;
      }

      document.title = doc.title;

      // Replace <main> content
      const range = document.createRange();
      range.selectNode(currentMain);
      const documentFragment = range.createContextualFragment(
        newMain.innerHTML ?? "",
      );

      currentMain.innerHTML = "";
      currentMain.append(documentFragment);

      initJsBasedOnPage(url);

      window.scrollTo({ top: 0, behavior: "instant" });
    },
  });
}
