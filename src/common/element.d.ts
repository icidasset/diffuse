export type HtmlTagFunction = (
  strings: string[] | ArrayLike<string>,
  ...values: unknown[]
) => string;

export type RenderArg<State> = { html: HtmlTagFunction; state: State };
