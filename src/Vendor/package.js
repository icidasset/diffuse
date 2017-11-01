self.jsmediatags = require("jsmediatags");
self.XhrFileReader = require("jsmediatags/build2/XhrFileReader");

self._ = require("1-liners");
self.blockstack = require("blockstack");
self.camelcase = require("camelcase");
self.elasticlunr = require("elasticlunr");
self.RemoteStorage = require("remotestoragejs");

if (self.document) {
  self.tocca = require("tocca");
}
