//
// User
// (づ｡◕‿‿◕｡)づ
//
// Related to the user layer.


// @ts-ignore
import * as TaskPort from "elm-taskport"

import type { App } from "./elm/types"

import * as crypto from "../crypto"

import { decryptIfNeeded, encryptIfPossible, SECRET_KEY_LOCATION } from "./common"
import { parseJsonIfNeeded, toCache } from "./common"


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


async function remoteStorage(userAddress: string, token: string) {
  if (!rs) {
    const { default: RemoteStorage } = await import("remotestoragejs")

    rs = new RemoteStorage({ cache: false })
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

function registerPorts(app: App) {
  Object.keys(ports).forEach(name => {
    const fn = ports[ name ](app)
    app.ports[ name ].subscribe(fn)
  })
}

function registerTaskPorts() {
  Object.keys(taskPorts).forEach(name => {
    const fn = taskPorts[ name ]
    TaskPort.register(name, fn)
  })
}


export const TaskPorts = { register: registerTaskPorts }
export const Ports = { register: registerPorts }
