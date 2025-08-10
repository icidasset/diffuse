import { METHODS } from "./constants";

export type Method = (typeof METHODS)[number];
export type List<M extends Method = Method> = Map<string, ListItem<M>>;
export type ListItem<M> = { activated: boolean; icon: string; method: M; title: string };
