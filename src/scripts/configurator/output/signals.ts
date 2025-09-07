import { effect, signal } from "@scripts/spellcaster";

import type { Method } from "./types";
import { DEFAULT_METHOD, LOCALSTORAGE_KEY, METHODS } from "./constants";

export const stored = localStorage.getItem(LOCALSTORAGE_KEY);
export const active = signal<Method>(
  stored && METHODS.includes(stored as Method) ? (stored as Method) : DEFAULT_METHOD,
);

// 🚀

// storage().then((s) => {
//   const stored = s.getItem(LOCALSTORAGE_KEY);
//   active(stored && METHODS.includes(stored as Method) ? (stored as Method) : DEFAULT_METHOD);
// });

// EFFECT

effect(() => {
  const method = active();
  storage().then((s) => s.setItem(LOCALSTORAGE_KEY, method));
});

async function storage() {
  // const hasStorageAccess =
  //   (await document.hasStorageAccess()) ||
  //   (
  //     await navigator.permissions.query({
  //       name: "storage-access",
  //     })
  //   ).state === "granted";

  // if (hasStorageAccess) {
  //   // @ts-ignore
  //   const handle: any = await document.requestStorageAccess({
  //     localStorage: true,
  //   });

  //   return handle ? handle.localStorage : localStorage;
  // } else {
  //   return localStorage;
  // }
  return localStorage;
}
