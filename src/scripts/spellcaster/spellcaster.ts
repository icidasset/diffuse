import { computed } from "alien-signals";

export type Props = Record<string, any> | (() => Record<string, any>);

/**
 * A signal is a zero-argument function that returns a value.
 * Reactive signals created with `signal()` will cause reactive contexts
 * to automatically re-execute when the signal changes.
 * Constant signals can be modeled as zero-argument functions that
 * return a constant value.
 */
export type Signal<T> = () => T;

/**
 * Is value a signal-like function?
 * A signal is any zero-argument function.
 */
export const isSignal = (value: any): value is Signal<any> =>
  typeof value === "function" && value.length === 0;

/** Sample a value that may be a signal, or just an ordinary value */
export const sample = <T>(value: T | Signal<T>): T => (isSignal(value) ? value() : value);

/**
 * Transform a signal, returning a computed signal that takes values until
 * the given signal returns null. Once the given signal returns null, the
 * signal is considered to be complete and no further updates will occur.
 *
 * This utility is useful for signals representing a child in a dynamic
 * collection of children, where the child may cease to exist.
 * A computed signal looks up the child, returns null if that child no longer
 * exists. This completes the signal and breaks the connection with upstream
 * signals, allowing the child signal to be garbaged.
 */
export const takeValues = <T>(maybeSignal: Signal<T | null | undefined>) => {
  const initial = maybeSignal();

  if (initial == null) {
    throw new TypeError("Signal initial value cannot be null");
  }

  let state = initial;
  let isComplete = false;

  return computed(() => {
    if (isComplete) {
      return state;
    }

    const next = maybeSignal();

    if (next != null) {
      state = next;
      return state;
    } else {
      isComplete = true;
      return state;
    }
  });
};
