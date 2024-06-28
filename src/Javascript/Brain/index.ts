//
// Brain
// 🧠
//
// This worker is responsible for everything non-UI.

import * as Application from "./application"
import * as Artwork from "./artwork"
import * as Processing from "./processing"
import * as Search from "./search"
import * as User from "./user"
import * as TaskPorts from "./task-ports"
import * as Tracks from "./tracks"
import * as UI from "./ui"


// 🚀

TaskPorts.register()
User.TaskPorts.register()

const app = Application.load()
const brain = self as unknown as Worker

// 🖼️

UI.link(brain, app)

// ⚡
Artwork.init(app)
Processing.init(app)
Search.init(app)
Tracks.init(app)

User.Ports.register(app)

// 🛫

brain.postMessage({ action: "READY" })
