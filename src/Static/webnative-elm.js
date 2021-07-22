// TODO: Remove once fixed in webnative-elm
//       https://github.com/fission-suite/webnative-elm/issues/17


const DEFAULT_PORT_NAMES = {
  incoming: "webnativeRequest",
  outgoing: "webnativeResponse"
}


;(function (root, factory) {
  if (typeof exports === "object" && typeof exports.nodeName !== "string") {
    // CommonJS
    factory(exports, require("webnative"))
  } else {
    // Browser globals
    factory((root.webnativeElm = {}), root.webnative)
  }

}(typeof self !== "undefined" ? self : this, function (exports, wn) {

    let fs
    const builtInGetFs = () => fs


    /**
     * Handle request.
     */
    exports.request = function({
      app,
      request,
      getFs = builtInGetFs,
      portNames = DEFAULT_PORT_NAMES
    }) {
      switch (request.context) {
        case "WEBNATIVE": return webnativeRequest({ app, portNames, request })
        case "WNFS": return wnfsRequest({ app, getFs, portNames, request })
      }

      return { getFs: getFs, portNames: portNames }
    }


    /**
     * Setup the ports for our Elm app.
     */
    exports.setup = function ({
      app,
      getFs = builtInGetFs,
      portNames = DEFAULT_PORT_NAMES,
      webnative
    }) {
      if (webnative) {
        wn = webnative
      }

      if (!wn) {
        throw new Error("Failed to load webnative")
      }

      if (!app.ports || !app.ports[portNames.incoming]) {
        console.warn(`Couldn't find the incoming Elm port for webnative named "${portNames.incoming}". Could be that you haven't used the port yet, dead code elimination.`)
        return
      }

      if (!app.ports[portNames.outgoing]) {
        console.warn(`Not sending webnative responses back to your Elm app, because the outgoing port named "${portNames.outgoing}" was not found. Could be that you haven't used the port yet, dead code elimination.`)
      }

      app.ports[portNames.incoming].subscribe(request => {
        exports.request({ request, app, getFs, portNames })
      })

      return { getFs: getFs, portNames: portNames }
    }


    /**
     * Handle webnative request.
     */
    function webnativeRequest({
      app, portNames, request
    }) {
      Promise.resolve(wn[request.method](
        ...request.arguments

      )).then(result => {
        switch (request.method) {
          case "redirectToLobby": return;
          case "loadFileSystem": fs = result; break;
          case "initialise": fs = result.fs; break;
        }

        // Report back to Elm
        if (app.ports[portNames.outgoing]) app.ports[portNames.outgoing].send({
          tag: request.tag,
          error: null,
          method: request.method,
          data: result,
          context: request.context
        })

      }).catch(err => {
        if (app.ports[portNames.outgoing]) app.ports[portNames.outgoing].send({
          tag: request.tag,
          error: err.message || err,
          method: request.method,
          data: null,
          context: request.context
        })

      })
    }


    /**
     * Handle WNFS request.
     */
    function wnfsRequest({
      app, getFs, portNames, request
    }) {
      const method = request.method.replace(/_utf8$/, "")

      if (request.method === "write") {
        request.arguments = [
          request.arguments[0],
          Uint8Array.from(request.arguments[1])
        ]
      }

      Promise.resolve(getFs()).then(fs => fs[method](
        ...request.arguments

      )).then(data => {
        if (app.ports[portNames.outgoing]) app.ports[portNames.outgoing].send({
          tag: request.tag,
          error: null,
          method: request.method,
          data: data.root ? null : (data.buffer ? Array.from(data) : data),
          context: request.context
        })

      }).catch(err => {
        if (app.ports[portNames.outgoing]) app.ports[portNames.outgoing].send({
          tag: request.tag,
          error: err.message || err,
          method: request.method,
          data: null,
          context: request.context
        })

      })
    }

}))
