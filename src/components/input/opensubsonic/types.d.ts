import type { SignalReader } from "@common/signal.d.ts";

// https://opensubsonic.netlify.app/docs/api-reference/
export type Server = {
  apiKey?: string;
  host: string;
  password?: string;
  tls: boolean;
  username?: string;
};

export type State = {
  servers: SignalReader<Record<string, Server>>;
};
