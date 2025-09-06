import { effect, signal } from "@scripts/spellcaster";

import type { Method } from "./types";
import { DEFAULT_METHOD, LOCALSTORAGE_KEY, METHODS } from "./constants";

export const stored = localStorage.getItem(LOCALSTORAGE_KEY);
export const active = signal<Method>(
  stored && METHODS.includes(stored as Method) ? (stored as Method) : DEFAULT_METHOD,
);

effect(() => {
  localStorage.setItem(LOCALSTORAGE_KEY, active());
});
