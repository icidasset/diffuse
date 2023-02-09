//
// User
// (づ｡◕‿‿◕｡)づ
//
// Related to the user layer.


// @ts-ignore
import * as TaskPort from "elm-taskport"
import { APP_INFO, WEBNATIVE_CONFIG } from "../common"

import * as crypto from "../crypto"

import { decryptIfNeeded, encryptIfPossible, SECRET_KEY_LOCATION } from "./common"
import { parseJsonIfNeeded, removeCache, toCache } from "./common"


const ports: Record<string, any> = {}
const taskPorts: Record<string, any> = {}


// Crypto
// ======

taskPorts.fabricateSecretKey = async passphrase => {
  const data = await crypto.keyFromPassphrase(passphrase)
  return toCache(SECRET_KEY_LOCATION, data)
}



// Dropbox
// -------

taskPorts.fromDropbox = ({ fileName, token }) => {
  return fetch("https://content.dropboxapi.com/2/files/download", {
    method: "POST",
    headers: {
      "Authorization": "Bearer " + token,
      "Dropbox-API-Arg": JSON.stringify({ path: "/" + fileName })
    }
  })
    .then(r => r.ok ? r.text() : r.json())
    .then(r => r.error ? null : r)
    .then(decryptIfNeeded)
    .then(parseJsonIfNeeded)
}


taskPorts.toDropbox = async ({ fileName, data, token }) => {
  const json = JSON.stringify(data)
  const params = {
    path: "/" + fileName,
    mode: "overwrite",
    mute: true
  }

  return fetch("https://content.dropboxapi.com/2/files/upload", {
    method: "POST",
    headers: {
      "Authorization": "Bearer " + token,
      "Content-Type": "application/octet-stream",
      "Dropbox-API-Arg": JSON.stringify(params)
    },
    body: await encryptIfPossible(json)
  })
}



// Fission
// -------

let session, wn


taskPorts.fromFission = async ({ fileName, includePublicData }) => {
  await constructFission()

  // Private data
  const privatePath = wn.path.appData(APP_INFO, wn.path.file(fileName))
  const privateData = await session.fs.exists(privatePath)
    ? session.fs.read(privatePath)
      .then(bytes => new TextDecoder().decode(bytes))
      .then(parseJsonIfNeeded)
    : null

  // If public data and working with arrays
  if (includePublicData && Array.isArray(privateData)) {
    const publicPath = {
      file: privatePath.file.map((a: string, idx: number) => {
        return idx === 0
          ? "public"
          : a
      })
    }

    const publicData = await session.fs.exists(publicPath)
      ? session.fs.read(publicPath)
        .then(bytes => new TextDecoder().decode(bytes))
        .then(parseJsonIfNeeded)
      : null

    return publicData
      ? [ ...privateData, ...publicData ]
      : privateData

    // Otherwise
  } else {
    return privateData

  }
}


taskPorts.toFission = async ({ data, fileName, savePublicData }) => {
  console.log("toFission", fileName, data)
  await constructFission()

  // Data identifying
  const privatePath = wn.path.appData(APP_INFO, wn.path.file(fileName))
  const isDataObject = typeof data === "object" && !!data.data

  if (!isDataObject) {
    await session.fs.write(
      privatePath,
      new TextEncoder().encode(JSON.stringify(data))
    )

    await session.fs.publish()

    return
  }

  // Group data
  const [ privateData, publicData ] = Array.isArray(data.data) && savePublicData
    ? data.data.reduce(
      ([ priv, pub ], item) => {
        return item.public
          ? [ priv, [ ...pub, item ] ]
          : [ [ ...priv, item ], pub ]
      },
      [ [], [] ]
    )
    : [ data.data, null ]

  // Private data
  await session.fs.write(
    privatePath,
    new TextEncoder().encode(JSON.stringify({ ...data, data: privateData }))
  )

  // Public data
  if (publicData) {
    const publicPath = {
      file: privatePath.file.map((a: string, idx: number) => {
        return idx === 0
          ? "public"
          : a
      })
    }

    await session.fs.write(
      publicPath,
      new TextEncoder().encode(JSON.stringify({ ...data, data: publicData }))
    )
  }

  // Publish
  await session.fs.publish()
}


async function constructFission() {
  if (session) return Promise.resolve()

  importScripts("vendor/webnative.min.js")

  wn = (self as any).webnative

  const program = await wn.program({
    ...WEBNATIVE_CONFIG,
    fileSystem: { loadImmediately: false }
  })

  session = program.session

  if (!session) {
    await removeCache("SYNC_METHOD")
    location.reload()
    throw new Error("Failed to load Webnative session")
  }

  session.fs = await program.fileSystem.load(session.username)
  if (!session.fs) throw new Error("Did not load Webnative file system")
}


ports.deconstructFission = _app => _ => {
  if (!session) return
  session.destroy()
  session = undefined
  wn = undefined
}



// IPFS
// ----

const IPFS_ROOT = "/Applications/Diffuse/"


taskPorts.fromIpfs = ({ apiOrigin, fileName }) => {
  const path = IPFS_ROOT + fileName

  return fetch(apiOrigin + "/api/v0/files/read?arg=" + encodeURIComponent(path), { method: "POST" })
    .then(r => r.ok ? r.text() : r.json())
    .then(r => r.Code === 0 ? null : r)
    .then(decryptIfNeeded)
    .then(parseJsonIfNeeded)
}


taskPorts.toIpfs = ({ apiOrigin, fileName, data }) => {
  const json = JSON.stringify(data)
  const params = new URLSearchParams({
    arg: IPFS_ROOT + fileName,
    create: "true",
    offset: "0",
    parents: "true",
    truncate: "true"
  }).toString()

  return encryptIfPossible(json).then(possiblyEncryptedData => {
    const formData = new FormData()

    formData.append("data", possiblyEncryptedData)

    return fetch(
      apiOrigin + "/api/v0/files/write?" + params,
      { method: "POST", body: formData }
    )
  })
}



// Remote Storage
// --------------

let rs
let rsClient


function remoteStorage(userAddress: string, token: string) {
  if (!rs) {
    importScripts("vendor/remotestorage.min.js")

    rs = new (self as any).RemoteStorage({ cache: false })
    rs.access.claim("diffuse", "rw")

    rsClient = rs.scope("/diffuse/")

    return new Promise(resolve => {
      rs.on("connected", resolve)
      rs.connect(userAddress, token)
    })

  } else {
    return Promise.resolve()

  }
}


ports.deconstructRemoteStorage = _app => _ => {
  rs = null
  rsClient = null
}


taskPorts.fromRemoteStorage = ({ fileName, userAddress, token }) => {
  return remoteStorage(userAddress, token)
    .then(_ => rsClient.getFile(fileName))
    .then(r => r.data)
    .then(decryptIfNeeded)
    .then(parseJsonIfNeeded)
}


taskPorts.toRemoteStorage = ({ data, fileName, userAddress, token }) => {
  const json = JSON.stringify(data)

  return remoteStorage(userAddress, token)
    .then(_ => encryptIfPossible(json))
    .then(data => rsClient.storeFile("application/json", fileName, data))
}



// EXPORT
// ======

export function setupPorts(app) {
  Object.keys(ports).forEach(name => {
    const fn = ports[ name ](app)
    app.ports[ name ].subscribe(fn)
  })
}

export function setupTaskPorts() {
  Object.keys(taskPorts).forEach(name => {
    const fn = taskPorts[ name ]
    TaskPort.register(name, fn)
  })
}
