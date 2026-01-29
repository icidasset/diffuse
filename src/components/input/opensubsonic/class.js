import { SubsonicAPI } from "subsonic-api";

export class SubsonicAPIWithoutFetch extends SubsonicAPI {
  /**
   * @param {import("./types.d.ts").SubsonicConfig} config
   */
  constructor(config) {
    super({
      ...config,
      fetch: async (path, params) => {
        if (path.toString().includes("/rest/stream.view")) {
          return await fetch(path, { ...params, method: "HEAD" });
        } else {
          return await fetch(path, params);
        }
      },
    });
    this.config = config;
  }
}
