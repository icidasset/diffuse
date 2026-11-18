import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as PUBLISH_NAME } from "~/components/supplement/atproto-publish/element.js";

const MARKER = "data-atproto-publish";

/**
 * Mount the Publish on the Atmosphere element beside whatever output is
 * selected.
 *
 * The element waits for the output to render and reuses the public AT Protocol
 * output's OAuth session, so nothing is published until the user has connected
 * their account.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  if (document.querySelector(`${PUBLISH_NAME}[${MARKER}]`)) return;

  const publisher = document.createElement(PUBLISH_NAME);
  publisher.setAttribute(MARKER, "");
  publisher.setAttribute("group", foundation.GROUP);
  publisher.setAttribute("output-selector", output.selector);
  document.body.append(publisher);
});
