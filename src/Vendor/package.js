self.jsmediatags = require("jsmediatags");
self.XhrFileReader = require("jsmediatags/build2/XhrFileReader");

self.blockstack = require("blockstack");
self.camelcase = require("camelcase");
self.encoding = require("text-encoding");
self.lunr = require("lunr");
self.RemoteStorage = require("remotestoragejs/release/remotestorage");

if (self.document) {
  self.tocca = require("tocca");
  self.x0p = require("x0popup");
  require("pepjs");
}
