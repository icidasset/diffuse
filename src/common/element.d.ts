export type HtmlTagFunction = (
  strings: string[] | ArrayLike<string>,
  ...values: unknown[]
) => string;

type MorphOptions = {
  getNodeKey?: (node: Node) => unknown;
  onBeforeNodeAdded?: (node: Node) => false | Node;
  onNodeAdded?: (node: Node) => void;
  onBeforeElUpdated?: (fromEl: HTMLElement, toEl: HTMLElement) => boolean;
  onElUpdated?: (el: HTMLElement) => void;
  onBeforeNodeDiscarded?: (node: Node) => boolean;
  onNodeDiscarded?: (node: Node) => void;
  onBeforeElChildrenUpdated?: (
    fromEl: HTMLElement,
    toEl: HTMLElement,
  ) => boolean;
  skipFromChildren?: (fromEl: HTMLElement) => boolean;
  addChild?: (parent: HTMLElement, child: HTMLElement) => void;
  childrenOnly?: boolean;
};

export type RenderArg<State> = { html: HtmlTagFunction; state: State };
