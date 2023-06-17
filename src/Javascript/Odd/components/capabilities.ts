import * as Uint8arrays from "uint8arrays"

import * as Base64 from "@oddjs/odd/common/base64"
import * as Capabilities from "@oddjs/odd/capabilities"
import * as Crypto from "@oddjs/odd/components/crypto/implementation"
import * as Depot from "@oddjs/odd/components/depot/implementation"
import * as DID from "@oddjs/odd/did/index"
import * as Fission from "@oddjs/odd/common/fission"
import * as Path from "@oddjs/odd/path/index"
import * as TypeChecks from "@oddjs/odd/common/type-checks"
import * as Ucan from "@oddjs/odd/ucan/index"

import { Implementation, RequestOptions } from "@oddjs/odd/components/capabilities/implementation"
import { Maybe } from "@oddjs/odd/common/types"
import { VERSION } from "@oddjs/odd/common/version"


// 🧩


export type Dependencies = {
  crypto: Crypto.Implementation
  depot: Depot.Implementation
}



// 🛠


export async function collect(
  endpoints: Fission.Endpoints,
  dependencies: Dependencies
): Promise<Maybe<Capabilities.Capabilities>> {
  const url = new URL(self.location.href)
  const username = url.searchParams.get("username") ?? ""
  if (!username) return null

  const info = await retry(
    () => getClassifiedViaPostMessage(endpoints, dependencies.crypto),
    {
      tries: 20,
      timeout: 60000,
      timeoutMessage: "Trying to retrieve UCAN(s) and readKey(s) from the auth lobby timed out after 60 seconds."
    }
  )

  const secrets = await translateClassifiedInfo(dependencies, info)

  if (!secrets) {
    throw new Error("Failed to retrieve secrets from lobby url parameters")
  }

  url.searchParams.delete("authorised")
  url.searchParams.delete("cancelled")
  url.searchParams.delete("newUser")
  url.searchParams.delete("username")

  history.replaceState(null, document.title, url.toString())

  return { ...secrets, username }
}


/**
 * Redirects to a lobby.
 *
 * NOTE: Only works on the main thread, as it uses `window.location`.
 */
export async function request(
  endpoints: Fission.Endpoints,
  dependencies: Dependencies,
  options: RequestOptions = {}
): Promise<void> {
  const { permissions } = options

  const app = permissions?.app
  const fs = permissions?.fs
  const platform = permissions?.platform
  const raw = permissions?.raw
  const sharing = permissions?.sharing

  const exchangeDid = await DID.exchange(dependencies.crypto)
  const writeDid = await DID.write(dependencies.crypto)
  const sharedRepo = false
  const redirectTo = options.returnUrl || window.location.href

  // Compile params
  const params = [
    [ "didExchange", exchangeDid ],
    [ "didWrite", writeDid ],
    [ "redirectTo", redirectTo ],
    [ "sdk", VERSION.toString() ],
    [ "sharedRepo", sharedRepo ? "t" : "f" ],
    [ "sharing", sharing ? "t" : "f" ]

  ].concat(
    app ? [ [ "appFolder", `${app.creator}/${app.name}` ] ] : [],
    fs?.private ? fs.private.map(p => [ "privatePath", Path.toPosix(p, { absolute: true }) ]) : [],
    fs?.public ? fs.public.map(p => [ "publicPath", Path.toPosix(p, { absolute: true }) ]) : [],
    raw ? [ [ "raw", Base64.urlEncode(JSON.stringify(raw)) ] ] : [],
    options.extraParams ? Object.entries(options.extraParams) : []

  ).concat((() => {
    const apps = platform?.apps

    switch (typeof apps) {
      case "string": return [ [ "app", apps ] ]
      case "object": return apps.map(a => [ "app", a ])
      default: return []
    }

  })())

  // And, go!
  window.location.href = endpoints.lobby + "?" +
    params
      .map(([ k, v ]) => encodeURIComponent(k) + "=" + encodeURIComponent(v))
      .join("&")
}



// COLLECTION HELPERS


type LobbyClassifiedInfo = {
  sessionKey: string
  secrets: string
  iv: string
}

type LobbySecrets = {
  fs: Record<string, { key: string; bareNameFilter: string }>
  ucans: string[]
}

async function getClassifiedViaPostMessage(
  endpoints: Fission.Endpoints,
  crypto: Crypto.Implementation
): Promise<LobbyClassifiedInfo> {
  const didExchange = await DID.exchange(crypto)
  const iframe: HTMLIFrameElement = await new Promise(resolve => {
    const iframe = document.createElement("iframe")
    iframe.id = "odd-secret-exchange"
    iframe.style.width = "0"
    iframe.style.height = "0"
    iframe.style.border = "none"
    iframe.style.display = "none"
    document.body.appendChild(iframe)

    iframe.onload = () => {
      resolve(iframe)
    }

    iframe.src = `${endpoints.lobby}/exchange.html`
  })

  return new Promise((resolve, reject) => {
    function stop() {
      globalThis.removeEventListener("message", listen)
      document.body.removeChild(iframe)
      reject()
    }

    function listen(event: MessageEvent<string>) {
      if (new URL(event.origin).host !== new URL(endpoints.lobby).host) return stop()
      if (event.data == null) return stop()

      let classifiedInfo

      try {
        classifiedInfo = JSON.parse(event.data)
      } catch {
        stop()
      }

      if (!isLobbyClassifiedInfo(classifiedInfo)) stop()
      globalThis.removeEventListener("message", listen)

      try { document.body.removeChild(iframe) } catch { }

      resolve(classifiedInfo)
    }

    globalThis.addEventListener("message", listen)

    if (iframe.contentWindow == null) {
      throw new Error("Can't import UCANs & readKey(s): No access to its contentWindow")
    }

    const message = {
      webnative: "exchange-secrets",
      didExchange
    }

    iframe.contentWindow.postMessage(message, iframe.src)
  })
}

function isLobbyClassifiedInfo(obj: unknown): obj is LobbyClassifiedInfo {
  return TypeChecks.isObject(obj)
    && TypeChecks.isString(obj.sessionKey)
    && TypeChecks.isString(obj.secrets)
    && TypeChecks.isString(obj.iv)
}

function isLobbySecrets(obj: unknown): obj is LobbySecrets {
  return TypeChecks.isObject(obj)
    && TypeChecks.isObject(obj.fs)
    && Object.values(obj.fs).every(a => TypeChecks.hasProp(a, "key") && TypeChecks.hasProp(a, "bareNameFilter"))
    && Array.isArray(obj.ucans)
    && obj.ucans.every(a => TypeChecks.isString(a))
}

async function translateClassifiedInfo(
  { crypto }: Dependencies,
  classifiedInfo: LobbyClassifiedInfo
): Promise<{ fileSystemSecrets: Capabilities.FileSystemSecret[]; ucans: Ucan.Ucan[] }> {
  // Extract session key
  const rawSessionKey = await crypto.keystore.decrypt(
    Uint8arrays.fromString(classifiedInfo.sessionKey, "base64pad")
  )

  // The encrypted session key and read keys can be encoded in both UTF-16 and UTF-8.
  // This is because keystore-idb uses UTF-16 by default, and that's what the ODD SDK used before.
  // ---
  // This easy way of detection works because the decrypted session key is encoded in base 64.
  // That means it'll only ever use the first byte to encode it, and if it were UTF-16 it would
  // split up the two bytes. Hence we check for the second byte here.
  const isUtf16 = rawSessionKey[ 1 ] === 0

  const sessionKey = isUtf16
    ? Uint8arrays.fromString(
      new TextDecoder("utf-16").decode(rawSessionKey),
      "base64pad"
    )
    : rawSessionKey

  // Decrypt secrets
  const secretsStr = await crypto.aes.decrypt(
    Uint8arrays.fromString(classifiedInfo.secrets, "base64pad"),
    sessionKey,
    Crypto.SymmAlg.AES_GCM,
    Uint8arrays.fromString(classifiedInfo.iv, "base64pad")
  )

  const secrets: unknown = JSON.parse(
    Uint8arrays.toString(secretsStr, "utf8")
  )

  if (!isLobbySecrets(secrets)) throw new Error("Invalid secrets received")

  const fileSystemSecrets: Capabilities.FileSystemSecret[] =
    isLobbySecrets(secrets)
      ? Object
        .entries(secrets.fs)
        .map(([ posixPath, { bareNameFilter, key } ]) => {
          return {
            bareNameFilter: bareNameFilter,
            path: Path.fromPosix(posixPath),
            readKey: Uint8arrays.fromString(key, "base64pad")
          }
        })
      : []

  const ucans: Ucan.Ucan[] = secrets.ucans.map(
    (u: string) => Ucan.decode(u)
  )

  return {
    fileSystemSecrets,
    ucans,
  }
}



// HELPERS


async function retry<T>(
  action: () => Promise<T>,
  options: { tries: number; timeout: number; timeoutMessage: string }
): Promise<T> {
  return new Promise((resolve, reject) => {
    if (options.tries > 0) {
      const unoMas = () => {
        retry(action, { ...options, tries: options.tries - 1 })
      }

      const timeoutId = setTimeout(unoMas, options.timeout)

      action()
        .then(resolve, unoMas)
        .finally(() => clearTimeout(timeoutId))

    } else {
      reject(new Error(options.timeoutMessage))

    }
  })
}



// 🛳


export function implementation(
  dependencies: Dependencies
): Implementation {
  const endpoints = Fission.PRODUCTION

  return {
    collect: () => collect(endpoints, dependencies),
    request: (...args) => request(endpoints, dependencies, ...args)
  }
}
