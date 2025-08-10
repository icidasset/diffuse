import { signal } from "spellcaster";
import * as IDB from "idb-keyval";

import { fetchHandles, fetchHandlesList } from "./common";
import { IDB_HANDLES } from "./constants";

////////////////////////////////////////////
// SIGNALS
////////////////////////////////////////////
export const [mounts, setMounts] = signal(await fetchHandlesList());

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////
export const mount = async () => {
  await showDirectoryPicker()
    .then(async (handle) => {
      const existingHandles = await fetchHandles();
      const id = crypto.randomUUID();

      await handle.requestPermission({ mode: "read" });
      await IDB.set(IDB_HANDLES, { ...existingHandles, [id]: handle });
      setMounts(await fetchHandlesList());
    })
    .catch(() => {});
};

export const unmount = async (handleId: string) => {
  const handles = await fetchHandles();
  delete handles[handleId];
  await IDB.set(IDB_HANDLES, { ...handles });
  setMounts(await fetchHandlesList());
};
