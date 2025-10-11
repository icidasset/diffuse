import manifest from "../../../pages/input/s3/_manifest.json";

export const IDB_PREFIX = "@applets/input/s3";
export const IDB_BUCKETS = `${IDB_PREFIX}/buckets`;
export const SCHEME = manifest.input_properties.scheme;

export const ENCODINGS = {
  "\+": "%2B",
  "\!": "%21",
  '\"': "%22",
  "\#": "%23",
  "\$": "%24",
  "\&": "%26",
  "'": "%27",
  "\(": "%28",
  "\)": "%29",
  "\*": "%2A",
  "\,": "%2C",
  "\:": "%3A",
  "\;": "%3B",
  "\=": "%3D",
  "\?": "%3F",
  "\@": "%40",
};
