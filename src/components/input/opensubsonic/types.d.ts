// https://opensubsonic.netlify.app/docs/api-reference/
export type Server = {
  apiKey?: string;
  host: string;
  password?: string;
  tls: boolean;
  username?: string;
};

export interface SubsonicConfig {
  /** The base URL of the Subsonic server, e.g., https://demo.navidrome.org. */
  url: string;

  /** The authentication details to use when connecting to the server. */
  auth:
    | {
      username: string;
      password: string;
      apiKey?: never;
    }
    | {
      username?: never;
      password?: never;
      apiKey: string;
    };
}
