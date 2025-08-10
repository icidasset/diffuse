// https://opensubsonic.netlify.app/docs/api-reference/
export type Server = {
  apiKey?: string;
  host: string;
  password?: string;
  tls: boolean;
  username?: string;
};
