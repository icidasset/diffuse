import type { ElmPorts } from "./elm/types"

export { }

declare global {
  const BUILD_TIMESTAMP: string

  const Elm: { UI: ElmMain<ElmPorts> }
  const tocca: any
}
