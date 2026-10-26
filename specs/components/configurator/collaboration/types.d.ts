import type { DiffuseElement } from "~/common/element.js";
import type { CollaborationSourceElement } from "@specs/components/collaboration/types.d.ts";

export type CollaborationConfiguratorActions = {
  /** All registered sources, keyed by their hint prefix. */
  sources(): Record<string, CollaborationSourceElement>;

  /** The source for a collaborator hint, e.g. `atproto/handle`. */
  sourceFor(hint: string | undefined): CollaborationSourceElement | undefined;
};

export type CollaborationConfiguratorElement =
  & DiffuseElement
  & CollaborationConfiguratorActions;
