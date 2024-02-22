import type { ElmPorts } from "./elm"

export { }

declare global {
  const BUILD_TIMESTAMP: string

  const Elm: { UI: ElmMain<ElmPorts> }
  const tocca: any
}
