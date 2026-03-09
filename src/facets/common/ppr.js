import * as Build from "./build.js";
import * as Grid from "./grid.js";
import * as You from "./you.js";

/**
 * @param {URL} url
 */
async function initJsBasedOnPage(url) {
  const path = url.pathname.replace(/(\/$)/, "");

  Grid.insertToggleButtons();
  await Grid.monitorToggleButtonStates();

  switch (path) {
    case "/facets/build":
      Build.renderEditor();
      Build.handleBuildFormSubmit();
      Build.listenForExamplesEdit();
      await Build.editFacetFromURL();
      break;
    case "/facets/you":
      await You.renderList();
      break;
    default:
      break;
  }
}

initJsBasedOnPage(new URL(location.href));

// Partial page updates for facets navigation using the Navigation API.
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

  // Only intercept /facets/[section]/ paths (not deeper sub-paths like /facets/tools/*)
  const parts = url.pathname.split("/").filter(Boolean);
  if (parts[0] !== "facets") return;
  if (parts.length > 2) return;

  // Skip the loader page
  if (parts[1] === "l") return;

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
