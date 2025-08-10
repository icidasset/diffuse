import { type Signal, computed, effect, signal } from "spellcaster/spellcaster.js";
import { type ElementConfigurator, h, repeat, text } from "spellcaster/hyperscript.js";

import { applet, hs, reactive } from "@scripts/applet/common";
import { CUSTOM_KEY } from "./constants";
import { active, setActive } from "./signals";
import { connection } from "./connections";
import { context } from "./context";
import type { List, ListItem, Method } from "./types";
import { setContextData } from "./events";

// const h = (
//   tag: string,
//   props?: Record<string, any> | Signal<Record<string, any>>,
//   configure?: ElementConfigurator,
// ) => hs(tag, scope, props, configure);

////////////////////////////////////////////
// EFFECTS
////////////////////////////////////////////
reactive(
  context.scope,
  (data) => data.tracks.cacheId,
  () => {
    // Export data URI
    const dl = document.querySelector("#download");
    if (dl) {
      const json = JSON.stringify(context.data.tracks.collection, null, 2);
      const href = URL.createObjectURL(new Blob([json], { type: "application/json" }));
      dl.setAttribute("href", href);
    }
  },
);

// Mount + Unmount
async function mountStorageMethod(method: Method) {
  switch (method) {
    case "custom":
      setModalIsOpen(true);
      break;
    default:
      const conn = await connection(method);
      try {
        await conn.sendAction("mount", undefined, { timeoutDuration: 60000 });
        setActive(method);
      } catch (err) {
        const msg: string =
          err && typeof err === "object" && "message" in err ? `${err.message}` : `${err}`;
        if (msg.startsWith("[user] ")) alert(msg.slice(7));
      }
      break;
  }
}

async function unmountStorageMethod(method: Method) {
  const conn = await connection(method);
  conn.removeEventListener("data", setContextData);
  await conn.sendAction("unmount", undefined, { timeoutDuration: 60000 });
}

////////////////////////////////////////////
// LIST
////////////////////////////////////////////
const list = computed<List>(() => {
  const a = active();

  return new Map([
    [
      `browser-${a === "browser"}`,
      {
        title: "Browser storage",
        icon: "iconoir-app-window",
        method: "browser",
        activated: a === "browser",
      },
    ],
    [
      `device-${a === "device"}`,
      {
        title: "Device storage",
        icon: "iconoir-laptop",
        method: "device",
        activated: a === "device",
      },
    ],
    [
      `custom-${a === "custom"}`,
      {
        title: "Custom applet",
        icon: "iconoir-globe",
        method: "custom",
        activated: a === "custom",
      },
    ],
  ]);
});

const Item = (signal: Signal<ListItem<Method>>) => {
  const item = signal();

  const colorClass = item.activated ? "pico-color-jade-500" : "pico-color-grey-500";
  const icon = item.activated ? "iconoir-check-circle-solid" : "iconoir-check-circle";

  return h(
    "p",
    {
      onclick: clickHandler(item.method),
      style: "cursor: pointer",
    },
    [
      h("span", { className: "with-icon" }, [
        h("i", { className: item.icon }),
        h("strong", {}, text(item.title)),
      ]),
      h("br"),
      h("span", { className: `with-icon ${colorClass}` }, [
        h("i", { className: icon }),
        h("span", {}, text(item.activated ? "Active" : "Select")),
      ]),
    ],
  );
};

function clickHandler(method: Method) {
  return async () => {
    const currentlyActive = active();
    if (currentlyActive === method && currentlyActive !== "custom") return;
    if (currentlyActive) unmountStorageMethod(currentlyActive);
    await mountStorageMethod(method);
  };
}

const Options = computed(() => {
  return h("div", { id: "options" }, repeat(list, Item));
});

// Add to DOM
document.getElementById("options")?.replaceWith(Options());

////////////////////////////////////////////
// CUSTOM APPLET
////////////////////////////////////////////
type CustomAppletState = "waiting" | "connecting" | { error: string } | "connected";

const [modalIsOpen, setModalIsOpen] = signal(false);
const [customState, setCustomState] = signal<CustomAppletState>("waiting");

const Modal = () => {
  const Header = h("header", {}, [
    h("button", {
      attrs: { rel: "prev" },
      ariaLabel: "Close",
      onclick: close,
    }),
    h("p", {}, [
      h("strong", {}, [
        h("span", { className: "with-icon" }, [
          h("i", { className: "iconoir-globe" }),
          h("span", {}, text("Load a custom applet")),
        ]),
      ]),
    ]),
  ]);

  const Content = h("form", { onsubmit: submit }, [
    h("fieldset", { role: "group" }, [
      h("input", {
        type: "url",
        name: "url",
        placeholder: "https://applets.diffuse.sh/storage/output/indexed-db/",
        required: true,
        value: localStorage.getItem(CUSTOM_KEY) || "",
      }),
      h("input", { type: "submit", value: "Connect" }),
    ]),
    h("p", {}, [
      h("small", { className: "with-icon" }, (element) => {
        const comp = computed(() => {
          const s = customState();

          if (s === "connecting") {
            return [
              h("i", { className: "iconoir-ev-plug-charging" }),
              h("span", {}, text("Connecting ...")),
            ];
          } else if (typeof s !== "string") {
            return [
              h("i", { className: "iconoir-warning-circle" }),
              h("span", {}, text(`Error: ${s.error}`)),
            ];
          }

          return [h("span", {}, text("Enter the URL to the applet."))];
        });

        effect(() => {
          element.replaceChildren(...comp());
        });
      }),
    ]),
  ]);

  return h(
    "dialog",
    computed(() => ({ open: modalIsOpen() })),
    [h("article", {}, [Header, Content])],
  );
};

// Events
function close() {
  setModalIsOpen(false);
}

async function submit(event: SubmitEvent) {
  event.preventDefault();

  const input: HTMLInputElement | null = (event.target as HTMLFormElement).querySelector(
    `input[type="url"]`,
  );

  if (!input) return;

  const url = input.value;
  setCustomState("connecting");

  const apl = await applet(url).catch((err) => {
    setCustomState({ error: "Failed to connect" });
    throw err;
  });

  let missingAction;

  ["tracks", "mount", "unmount"].forEach((method) => {
    if (!apl.manifest.actions?.[method]) missingAction = method;
  });

  if (missingAction) {
    setCustomState({ error: `Applet is missing a required action: "${missingAction}"` });
    return;
  }

  localStorage.setItem(CUSTOM_KEY, url);
  await apl.sendAction("mount", undefined, { timeoutDuration: 60000 });

  setActive("custom");
  setModalIsOpen(false);
  setCustomState("waiting");
}

// Add to DOM
document.querySelector("main")?.appendChild(Modal());
