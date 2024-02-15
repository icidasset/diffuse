import type { ElmPorts } from "./elm/types"


export { }


declare const Elm: { Brain: ElmMain<ElmPorts> }
declare const BUILD_TIMESTAMP: string


declare module "elm-taskport" {
  const install: () => void
  const register: (a: string, b: (arg: any) => any) => void
}
