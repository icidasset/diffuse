import * as Output from "~/common/output.js";
import foundation from "~/common/facets/foundation.js";
import { facetFromURI } from "~/common/facets/utils.js";

////////////////////////////////////////////
// SAVE & FORK
////////////////////////////////////////////

document.body.addEventListener(
  "click",
  /**
   * @param {MouseEvent} event
   */
  async (event) => {
    const target = /** @type {HTMLElement} */ (event.target);
    const rel = target.getAttribute("rel");
    if (!rel) return;

    const uri = target.closest("li")?.getAttribute("data-uri");
    if (!uri) return;

    const name = target.closest("li")?.getAttribute("data-name");
    if (!name) return;

    switch (rel) {
      case "fork": {
        // TODO: Go to build page and load this facet into the editor.
        //
        // const facet = await facetFromURI({ name, uri }, { fetchHTML: true });
        // editFacet(facet);
        // document.querySelector("#build")?.scrollIntoView();
        break;
      }
      case "save": {
        const facet = await facetFromURI({ name, uri }, { fetchHTML: false });
        const out = foundation.orchestrator.output();

        await Output.waitUntilLoaded(out.facets);

        out.facets.save([
          ...out.facets.collection(),
          facet,
        ]);
        break;
      }
    }

    // @ts-ignore No types exist yet for this I think?
    target.closest("[popover]")?.hidePopover();
  },
);
