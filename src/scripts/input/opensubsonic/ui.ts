import { computed, effect, type Signal, signal } from "spellcaster";
import { type Props, repeat, tags, text } from "spellcaster/hyperscript.js";

import type { Server } from "./types.d.ts";
import { loadServers, saveServers, serverId } from "./common";

////////////////////////////////////////////
// UI
////////////////////////////////////////////
export const [servers, setServers] = signal<Record<string, Server>>(await loadServers());
const [form, setForm] = signal<{
  api_key?: string;
  host?: string;
  password?: string;
  username?: string;
}>({});

const serversMap = computed(() => {
  return new Map(Object.entries(servers()));
});

effect(() => {
  saveServers(servers());
});

////////////////////////////////////////////
// UI ~ SERVERS
////////////////////////////////////////////
const Server = (server: Signal<Server>) => {
  const onclick = () => {
    const b = server();
    const id = serverId(b);

    const col = { ...servers() };
    delete col[id];

    setServers(col);
  };

  return tags.li({ onclick, style: "cursor: pointer" }, text(server().host));
};

const ServerList = computed(() => {
  if (serversMap().size === 0) {
    return tags.p({ id: "servers" }, [tags.small({}, text("Nothing added so far."))]);
  }

  return tags.ul({ id: "servers" }, repeat(serversMap, Server));
});

effect(() => {
  document.querySelector("#servers")?.replaceWith(ServerList());
});

////////////////////////////////////////////
// UI ~ FORM
////////////////////////////////////////////
function addServer(event: Event) {
  event.preventDefault();

  const f = form();

  const server: Server = {
    apiKey: f.api_key,
    host: f.host?.replace(/^https?:\/\//, "").replace(/\/+$/, "") || "localhost:4533",
    username: f.username,
    tls: f.host?.startsWith("http://") || f.host?.startsWith("localhost") ? false : true,
    password: f.password,
  };

  setServers({
    ...servers(),
    [serverId(server)]: server,
  });
}

function Form() {
  return tags.form({ onsubmit: addServer }, [
    tags.fieldset({ className: "grid" }, [
      Input("host", "Server host", "my.opensubsonic.server:4747", { required: true }),
    ]),
    tags.fieldset({ className: "grid" }, [
      Input("username", "Server name", "username", { required: true }),
      Input("password", "Password", "password", { required: true, type: "password" }),
    ]),
    tags.fieldset({ className: "grid" }, [tags.input({ type: "submit", value: "Connect" }, [])]),
  ]);
}

function Input(name: string, label: string, placeholder: string, opts: Props = {}) {
  return tags.label({}, [
    tags.span({}, [
      tags.span({}, text(label)),
      tags.small({}, text("required" in opts ? "" : " (optional)")),
    ]),
    tags.input({
      ...opts,
      name,
      placeholder,
      oninput: (event: InputEvent) => formInput(name, (event.target as HTMLInputElement).value),
    }),
  ]);
}

function formInput(name: string, value: string) {
  setForm({ ...form(), [name]: value });
}

// 🚀
document.querySelector("#form")?.replaceWith(Form());
