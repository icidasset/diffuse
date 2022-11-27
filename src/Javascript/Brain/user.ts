//
// User
// (づ｡◕‿‿◕｡)づ
//
// Related to the user layer.


// @ts-ignore
import * as TaskPort from "elm-taskport"

import * as crypto from "../crypto"
import { WEBNATIVE_STAGING_ENV, WEBNATIVE_STAGING_MODE, identity } from "../common"

import { encryptIfPossible, SECRET_KEY_LOCATION } from "./common"
import { decryptIfNeeded, encryptWithSecretKey } from "./common"
import { fromCache, isLocalHost, parseJsonIfNeeded, reportError } from "./common"
import { sendJsonData, storageCallback, toCache } from "./common"


const ports: Record<string, any> = {}
const taskPorts: Record<string, any> = {}


// Crypto
// ======

ports.fabricateSecretKey = app => event => {
  crypto.keyFromPassphrase(event.data)
    .then(data => toCache(SECRET_KEY_LOCATION, data))
    .then(_ => {
      app.ports.fromAlien.send({
        tag: event.tag,
        data: null,
        error: null
      })
    })
    .catch(reportError(app, event))
}

taskPorts.fabricateSecretKey = async passphrase => {
  const data = await crypto.keyFromPassphrase(passphrase)
  return toCache(SECRET_KEY_LOCATION, data)
}



// Dropbox
// -------

ports.requestDropbox = app => event => { } // TODO: Remove
ports.toDropbox = app => event => { } // TODO: Remove

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

  const possiblyEncrypted = await encryptIfPossible(data)

  return fetch("https://content.dropboxapi.com/2/files/upload", {
    method: "POST",
    headers: {
      "Authorization": "Bearer " + token,
      "Content-Type": "application/octet-stream",
      "Dropbox-API-Arg": JSON.stringify(params)
    },
    body: possiblyEncrypted
  })


}



// Fission
// -------

let wn
let wnfs


ports.webnativeRequest = app => request => {
  const getFs = () => wnfs

  constructFission.call(self).then(() => {
    if (request.method === "loadFileSystem") {
      (self as any).webnative.loadFileSystem(...request.arguments).then(fs => {
        wnfs = fs
        app.ports.webnativeResponse.send({
          tag: request.tag,
          error: null,
          method: request.method,
          data: {},
          context: request.context
        })
      })
    } else {
      (self as any).webnativeElm.request({ app: app, getFs, request: request })
    }
  })
}


function constructFission() {
  if (wn) return Promise.resolve()

  importScripts("vendor/ipfs.min.js")
  importScripts("vendor/webnative.min.js")
  importScripts("vendor/webnative-elm.min.js")

  // Environment setup
  wn = (self as any).webnative

  if ([ "localhost", "nightly.diffuse.sh" ].includes(location.hostname)) {
    wn.setup.debug({ enabled: true })
  }

  let endpoints

  if (WEBNATIVE_STAGING_MODE) {
    endpoints = wn.setup.endpoints(WEBNATIVE_STAGING_ENV)
  } else {
    endpoints = wn.setup.endpoints({})
  }

  // Connect IPFS
  const peersPromise = fetch(`${endpoints.api}/ipfs/peers`)
    .then(r => r.json())
    .then(r => r.filter(p => p.includes("/wss/")))
    .catch(() => { throw new Error("💥 Couldn't start IPFS node, failed to fetch peer list") })

  return peersPromise.then(peers => {
    return (self as any).Ipfs.create({
      config: {
        Addresses: {
          Delegates: []
        },
        Bootstrap: [],
        Discovery: {
          webRTCStar: { enabled: false }
        }
      },
      preload: {
        enabled: false
      },
      libp2p: {
        config: {
          peerDiscovery: { autoDial: false }
        }
      }
    }).then(ipfs => {
      peers.forEach(peer => tryConnectingToIpfsPeer(ipfs, peer))
      wn.ipfs.set(ipfs)
    })
  })
}


ports.deconstructFission = _app => _ => {
  wn.leave({ withoutRedirect: true })
  wn = null
}


// TODO: This stuff is going to be moved into webnative.
//       Remove when possible.


const KEEP_ALIVE_INTERVAL =
  1 * 60 * 1000 // 1 minute

const BACKOFF_INIT = {
  retryNumber: 0,
  lastBackoff: 0,
  currentBackoff: 1000
}

const KEEP_TRYING_INTERVAL =
  5 * 60 * 1000 // 5 minutes

const latestPeerTimeoutIds = {}


function tryConnectingToIpfsPeer(ipfs, peer) {
  ipfs.libp2p.ping(peer).then(() => {
    return ipfs.swarm.connect(peer, 1 * 1000)
      .then(() => {
        console.log(`🪐 Connected to ${peer}`)
        setTimeout(() => keepAlive(ipfs, peer, BACKOFF_INIT), KEEP_ALIVE_INTERVAL)
      })
      .catch(() => {
        console.log(`🪓 Could not connect to ${peer}`)
        keepAlive(ipfs, peer, BACKOFF_INIT)
      })
  })
}


function keepAlive(ipfs, peer, backoff) {
  let timeoutId = 0

  if (backoff.currentBackoff < KEEP_TRYING_INTERVAL) {
    timeoutId = setTimeout(() => reconnect(ipfs, peer, backoff), backoff.currentBackoff)
  } else {
    timeoutId = setTimeout(() => reconnect(ipfs, peer, backoff), KEEP_TRYING_INTERVAL)
  }

  latestPeerTimeoutIds[ peer ] = timeoutId

  ipfs.libp2p.ping(peer).then(_ => {
    clearTimeout(timeoutId)

    if (timeoutId === latestPeerTimeoutIds[ peer ]) {
      setTimeout(() => keepAlive(ipfs, peer, BACKOFF_INIT), KEEP_ALIVE_INTERVAL)
    }
  }).catch(() => { })
}


function reconnect(ipfs, peer, backoff) {
  ipfs.swarm.disconnect(peer)
    .then(() => ipfs.swarm.connect(peer))
    .catch(() => { })

  if (backoff.currentBackoff < KEEP_TRYING_INTERVAL) {
    const nextBackoff = {
      retryNumber: backoff.retryNumber + 1,
      lastBackoff: backoff.currentBackoff,
      currentBackoff: backoff.lastBackoff + backoff.currentBackoff
    }

    keepAlive(ipfs, peer, nextBackoff)
  } else {
    keepAlive(ipfs, peer, backoff)
  }
}



// IPFS
// ----

const IPFS_ROOT = "/Applications/Diffuse/"


ports.requestIpfs = app => event => {
  const apiOrigin = event.data.apiOrigin
  const path = IPFS_ROOT + event.data.file

  fetch(apiOrigin + "/api/v0/files/read?arg=" + path, { method: "POST" })
    .then(r => r.ok ? r.text() : r.json())
    .then(r => r.Code === 0 ? null : r)
    .then(decryptIfNeeded)
    .then(sendJsonData(app, event))
    .catch(reportError(app, event))
}


ports.toIpfs = app => event => {
  const apiOrigin = event.data.apiOrigin
  const json = JSON.stringify(event.data.data)
  const params = new URLSearchParams({
    arg: IPFS_ROOT + event.data.file,
    create: "true",
    offset: "0",
    parents: "true",
    truncate: "true"
  }).toString()

  encryptWithSecretKey(json)
    .then(data => {
      const formData = new FormData()

      formData.append("data", data)

      return fetch(
        apiOrigin + "/api/v0/files/write?" + params,
        { method: "POST", body: formData }
      )
    })
    .then(storageCallback(app, event))
    .catch(reportError(app, event))
}



// Remote Storage
// --------------

let rs
let rsClient


function remoteStorage(event) {
  if (!rs) {
    importScripts("vendor/remotestorage.min.js")

    rs = new (self as any).RemoteStorage({ cache: false })
    rs.access.claim("diffuse", "rw")

    rsClient = rs.scope("/diffuse/")

    return new Promise(resolve => {
      rs.on("connected", resolve)
      rs.connect(event.data.userAddress, event.data.token)
    })

  } else {
    return Promise.resolve()

  }
}


function remoteStorageIsUnavailable(event) {
  return !navigator.onLine &&
    !isLocalHost(event.data.userAddress.replace(/^[^@]*@/, ""))
}


ports.deconstructRemoteStorage = _app => _ => {
  rs = null
  rsClient = null
}


ports.requestRemoteStorage = app => event => {
  const isOffline =
    remoteStorageIsUnavailable(event)

  const dataPromise =
    isOffline
      // TODO: Replace with local data syncing
      ? fromCache(event.tag + "_" + event.data.file)
      : remoteStorage(event)
        .then(_ => rsClient.getFile(event.data.file))
        .then(r => r.data)
        .then(decryptIfNeeded)

  dataPromise
    .then(sendJsonData(app, event))
    .catch(reportError(app, event))
}


ports.toRemoteStorage = app => event => {
  const json = JSON.stringify(event.data.data)
  const doEncryption = _ => encryptWithSecretKey(json)
  const isOffline = remoteStorageIsUnavailable(event)

  !isOffline && remoteStorage(event)
    .then(doEncryption)
    .then(data => rsClient.storeFile("application/json", event.data.file, data))
    .then(storageCallback(app, event))
    .catch(reportError(app, event))

  toCache(event.tag + "_" + event.data.file, event.data.data)
    .then(isOffline ? storageCallback(app, event) : identity)
    .catch(reportError(app, event))
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
