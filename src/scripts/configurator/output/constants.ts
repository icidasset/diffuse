export const METHODS = ["browser", "custom", "device"] as const;

export const CONNECTIONS = {
  browser: "/output/indexed-db/",
  custom: undefined,
  device: "/output/native-fs/",
};

export const DEFAULT_METHOD: (typeof METHODS)[number] = "browser";
export const LOCALSTORAGE_KEY = "applets/configurator/output/active-output";
export const CUSTOM_KEY = "applets/configurator/output/custom-applet";
