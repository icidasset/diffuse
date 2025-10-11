import type { ReactiveNode } from "alien-signals";

export interface Signal<T = any> extends ReactiveNode {
  currentValue: T;
  pendingValue: T;
}
