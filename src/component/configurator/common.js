import QS from "query-string";

/**
 * @param {Location} loc
 * @returns {Record<string, Worker>}
 */
export function connectionsFromQuery(loc) {
  const qs = QS.parse(loc.search);
  console.log(qs);

  return {};
}
