export type SignalReader<T> = () => T;
export type SignalWriter<T> = (t: T) => void;

export type Signal<T> = {
  get: SignalReader<T>;
  set: SignalWriter<T>;

  get value(): T;
  set value(t: T);
};
