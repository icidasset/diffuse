/**
 * The minimal DOM surface the output element needs to run in a worker.
 *
 * The Record Sync engine itself is DOM-free: the element only reads its
 * attributes, tracks `isConnected`, and adds lifecycle listeners. Supplying
 * just those lets the worker run the same, already-tested element instead of
 * maintaining a second copy of the merge/log logic.
 */

class ShimElement extends EventTarget {
  /** @type {Map<string, string>} */
  #attributes = new Map();

  shadowRoot = null;
  isConnected = false;
  localName = "shim";
  id = "";

  /** @param {string} name */
  getAttribute(name) {
    return this.#attributes.get(name) ?? null;
  }

  /** @param {string} name @param {unknown} value */
  setAttribute(name, value) {
    this.#attributes.set(name, String(value));
  }

  /** @param {string} name */
  hasAttribute(name) {
    return this.#attributes.has(name);
  }

  /** @param {string} name */
  removeAttribute(name) {
    this.#attributes.delete(name);
  }

  getAttributeNames() {
    return [...this.#attributes.keys()];
  }
}

class ShimDocument extends EventTarget {
  visibilityState = "visible";

  querySelector() {
    return null;
  }

  querySelectorAll() {
    return [];
  }

  /** @param {string} _name */
  createElement(_name) {
    return new ShimElement();
  }

  /** @param {string} text */
  createTextNode(text) {
    return { nodeType: 3, textContent: text };
  }

  /** @param {string} text */
  createComment(text) {
    return { nodeType: 8, textContent: text, data: text };
  }

  createTreeWalker() {
    return {
      currentNode: null,
      nextNode: () => null,
      previousNode: () => null,
    };
  }
}

const scope = /** @type {any} */ (globalThis);

scope.HTMLElement ??= ShimElement;
scope.document ??= new ShimDocument();
scope.customElements ??= {
  define() {},
  get() {
    return undefined;
  },
  whenDefined: async () => {},
};
scope.window ??= scope;
