//
// User
// (づ｡◕‿‿◕｡)づ
//
// Related to the user layer.


import * as crypto from "../crypto"
import { WEBNATIVE_PERMISSIONS, identity } from "../common"

import { SECRET_KEY_LOCATION } from "./common"
import { decryptIfNeeded, encryptWithSecretKey } from "./common"
import { fromCache, isLocalHost, reportError } from "./common"
import { sendData, sendJsonData, storageCallback, toCache } from "./common"


const ports = []


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



// Dropbox
// -------

ports.requestDropbox = app => event => {
  const params = {
    path: "/" + event.data.file
  }

  const dataPromise =
    !navigator.onLine
    ? fromCache(event.tag + "_" + event.data.file)
    : fetch("https://content.dropboxapi.com/2/files/download", {
        method: "POST",
        headers: {
          "Authorization": "Bearer " + event.data.token,
          "Dropbox-API-Arg": JSON.stringify(params)
        }
      })
        .then(r => r.ok ? r.text() : r.json())
        .then(r => r.error ? null : r)
        .then(decryptIfNeeded)

  dataPromise
    .then( sendJsonData(app, event) )
    .catch( reportError(app, event) )
}


ports.toDropbox = app => event => {
  const json = JSON.stringify(event.data.data)
  const reporter = reportError(app, event)
  const params = {
    path: "/" + event.data.file,
    mode: "overwrite",
    mute: true
  }

  navigator.onLine && encryptWithSecretKey(json)
    .then(data => {
      return fetch("https://content.dropboxapi.com/2/files/upload", {
        method: "POST",
        headers: {
          "Authorization": "Bearer " + event.data.token,
          "Content-Type": "application/octet-stream",
          "Dropbox-API-Arg": JSON.stringify(params)
        },
        body: data
      })
    })
    .then( storageCallback(app, event) )
    .catch(reporter)

  toCache(event.tag + "_" + event.data.file, event.data.data)
    .then( !navigator.onLine ? storageCallback(app, event) : identity )
    .catch(reporter)
}



// Fission
// -------

let wn
let wnfs

const PLAYLISTS_PATH = "private/Audio/Music/Playlists/"


function fission() {
  if (!wn) {
    importScripts("vendor/webnative.min.js")
    importScripts("vendor/ipfs-message-port-client.min.js")

    wn = self.webnative

    if ([ "localhost", "nightly.diffuse.sh" ].includes(location.hostname)) {
      wn.setup.debug({ enabled: true })
    }

    return (
      new Promise((resolve) => {
        const channel = new MessageChannel()

        channel.port1.onmessage = ({ ports }) => {
          resolve(ports[0])
        }

        self.postMessage(
          { action: "SETUP_WEBNATIVE" },
          [ channel.port2 ]
        )
      })
    )
      .then(port => self.IpfsMessagePortClient.from(port))
      .then(ipfs => wn.ipfs.set(ipfs))
      .then(_ => wn.loadFileSystem(WEBNATIVE_PERMISSIONS))
      .then(fs => wnfs = fs)

  } else {
    return Promise.resolve()

  }
}


ports.deconstructFission = _app => _ => {
  wn.leave({ withoutRedirect: true })
  wn = null
  wnfs = null
}


ports.requestFission = app => event => {
  fission()
    .then(() => {
      switch (event.data.file) {

        case "playlists.json":
          return wnfs.exists(PLAYLISTS_PATH)

        default:
          return wnfs.exists(wnfs.appPath([ event.data.file ]))

      }
    })
    .then(exists => {
      const sendJsonData_ = sendJsonData(app, event)
      if (!exists) return sendJsonData_(null)

      switch (event.data.file) {

        case "playlists.json":
          return wnfs.exists(PLAYLISTS_PATH).then(exists => {
            if (!exists) return wnfs
              .read(wnfs.appPath([ event.data.file ]))
              .then(a => a ? new TextDecoder().decode(a) : null)
              .then(sendJsonData_)

            return wnfs.ls(PLAYLISTS_PATH).then(result =>
              Object.values(result).map(r =>
                wnfs
                  .read(PLAYLISTS_PATH + r.name)
                  .then(p => new TextDecoder().decode(p))
                  .then(j => JSON.parse(j))
              )
            )
          })
          .then(promises =>
            Promise.all(promises)
          ).then(playlists =>
            sendData(app, event)(playlists)
          )

        default:
          return wnfs
            .read(wnfs.appPath([ event.data.file ]))
            .then(a => a ? new TextDecoder().decode(a) : null)
            .then(sendJsonData_)

      }
    })
    .catch( reportError(app, event) )
}


ports.toFission = app => event => {
  fission()
    .then(() => {
      switch (event.data.file) {

        case "playlists.json":
          return wnfs.exists(PLAYLISTS_PATH).then(exists => {
            if (exists) return null
            return wnfs.mkdir(PLAYLISTS_PATH)
          })

        default:
          return null

      }
    })
    .then(() => {
      let playlistFilenames

      switch (event.data.file) {

        case "playlists.json":
          playlistFilenames = event.data.data.map(playlist =>
            `${playlist.name}.json`
          )

          return wnfs.exists(PLAYLISTS_PATH).then(exists => {
            if (exists) return true
            return wnfs.mkdir(PLAYLISTS_PATH)

          }).then(_ =>
            wnfs.ls(PLAYLISTS_PATH)

          ).then(list =>
            // delete playlists that are no longer in the catalog
            Object.values(list).map(l => l.name).filter(name =>
              !playlistFilenames.includes(name)
            )

          ).then(playlistsToRemove =>
            Promise.all(playlistsToRemove.map(name =>
              wnfs.rm(`${PLAYLISTS_PATH}/${name}`)
            ))

          ).then(() =>
            // create/update playlists
            Promise.all(event.data.data.map(playlist => wnfs.write(
              `${PLAYLISTS_PATH}/${playlist.name}.json`,
              new Blob(
                [ JSON.stringify(playlist) ],
                { type: "text/plain" }
              )
            )))

          )

        default:
          return wnfs.write(
            wnfs.appPath([ event.data.file ]),
            new Blob(
              [ JSON.stringify(event.data.data) ],
              { type: "text/plain" }
            )
          )

      }
    })
    .then(() => wnfs.publish())
    .then( storageCallback(app, event) )
    .catch( reportError(app, event) )
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
    .then( sendJsonData(app, event) )
    .catch( reportError(app, event) )
}


ports.toIpfs = app => event => {
  const apiOrigin = event.data.apiOrigin
  const json = JSON.stringify(event.data.data)
  const params = new URLSearchParams({
    arg: IPFS_ROOT + event.data.file,
    create: true,
    offset: 0,
    parents: true,
    truncate: true
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
    .then( storageCallback(app, event) )
    .catch( reportError(app, event) )
}



// Legacy
// ------

ports.requestLegacyLocalData = app => event => {
  let oldIdx
  let key = location.hostname + ".json"

  oldIdx = indexedDB.open(key, 1)
  oldIdx.onsuccess = _ => {
    const old = oldIdx.result
    const tra = old.transaction([key], "readwrite")
    const req = tra.objectStore(key).get(key)

    req.onsuccess = _ => {
      if (req.result) sendJsonData(app, event)(req.result)
    }
  }
}



// Remote Storage
// --------------

let rs
let rsClient


function remoteStorage(event) {
  if (!rs) {
    importScripts("vendor/remotestorage.min.js")

    rs = new RemoteStorage({ cache: false })
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
    ? fromCache(event.tag + "_" + event.data.file)
    : remoteStorage(event)
        .then(_ => rsClient.getFile(event.data.file))
        .then(r => r.data)
        .then(decryptIfNeeded)

  dataPromise
    .then( sendJsonData(app, event) )
    .catch( reportError(app, event) )
}


ports.toRemoteStorage = app => event => {
  const json = JSON.stringify(event.data.data)
  const doEncryption = _ => encryptWithSecretKey(json)
  const isOffline = remoteStorageIsUnavailable(event)

  !isOffline && remoteStorage(event)
    .then(doEncryption)
    .then(data => rsClient.storeFile("application/json", event.data.file, data))
    .then( storageCallback(app, event) )
    .catch( reportError(app, event) )

  toCache(event.tag + "_" + event.data.file, event.data.data)
    .then( isOffline ? storageCallback(app, event) : identity )
    .catch( reportError(app, event) )
}



// EXPORT
// ======

export function setupPorts(app) {
  Object.keys(ports).forEach(name => {
    const fn = ports[name](app)
    app.ports[name].subscribe(fn)
  })
}
