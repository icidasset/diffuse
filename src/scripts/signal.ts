import { Signal } from "signal-polyfill";

// SIGNAL

export type Signal<T> = () => T;

export const signal = <T>(initial: T): [Signal<T>, (value: T) => void] => {
  const state = new Signal.State(initial);
  const get = () => state.get();
  const set = (value: T) => state.set(value);
  return [get, set];
};

// EFFECT

export const throttled = (
  job: () => void,
  queue: (callback: () => void) => void = queueMicrotask,
): (() => void) => {
  let isScheduled = false;

  const perform = () => {
    job();
    isScheduled = false;
  };

  const schedule = () => {
    if (!isScheduled) {
      isScheduled = true;
      queue(perform);
    }
  };

  return schedule;
};

const watcher = new Signal.subtle.Watcher(
  throttled(() => {
    for (const signal of watcher.getPending()) {
      signal.get();
    }
    watcher.watch();
  }),
);

export type Cancel = () => void;

export const effect = (perform: () => Cancel | void) => {
  let cleanup: Cancel | undefined;

  const signal = new Signal.Computed(() => {
    cleanup?.();
    cleanup = perform() ?? undefined;
  });

  watcher.watch(signal);
  signal.get();

  const dispose = () => {
    cleanup?.();
    watcher.unwatch(signal);
  };

  return dispose;
};
