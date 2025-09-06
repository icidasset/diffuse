import { computed, effect, type Signal } from "@scripts/spellcaster";
import { repeat, tags, text } from "@scripts/spellcaster/hyperscript.js";

import { mount, mounts, unmount } from "./mounting";
import { isSupported } from "./common";

////////////////////////////////////////////
// SIGNALS
////////////////////////////////////////////

// Mount button
document.getElementById("mount")?.addEventListener("click", () => {
  if (isSupported()) mount();
  else alert("The File System Access API is not supported on this platform.");
});

// Directories
const dirList = computed(() => {
  return new Map(
    mounts().map((mount) => {
      return [mount.id, mount];
    }),
  );
});

const Item = (signal: Signal<{ id: string; handle: FileSystemDirectoryHandle }>) => {
  const { id, handle } = signal();

  return tags.li({}, [
    tags.span(
      { onclick: () => unmount(id), style: "cursor: pointer;", title: "Click/tap to delete" },
      text(handle.name),
    ),
  ]);
};

const Directories = computed(() => {
  if (mounts().length === 0) {
    return tags.p({ id: "directories" }, [
      tags.small({}, [
        tags.em({}, text("No audio added yet, click the button below to add some.")),
      ]),
    ]);
  }

  return tags.ul({ id: "directories" }, repeat(dirList, Item));
});

// Add to DOM
effect(() => {
  document.getElementById("directories")?.replaceWith(Directories());
});
