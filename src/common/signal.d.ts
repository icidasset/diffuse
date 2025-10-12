import type { signal } from "alien-signals";

export type Signal<T> = ReturnType<typeof signal<T>>;
