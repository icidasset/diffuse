/**
 * Helpers for standing up in-process HTTP mock servers for integration tests.
 * Each helper returns the running `Deno.HttpServer` plus its bound port so
 * tests can construct URLs that point back at the mock.
 */

/**
 * Start a mock HTTP server and call the provided handler for each request.
 * Resolves with the running server and its port.
 *
 * @param {(req: Request, url: URL) => Response | Promise<Response>} handler
 * @returns {Promise<{ server: Deno.HttpServer; port: number }>}
 *
 * @example
 * ```ts
 * import { mockServer } from "@tests/common/server.ts";
 * import { expect } from "@std/expect";
 *
 * const { server, port } = await mockServer((_req, url) => {
 *   if (url.pathname === "/hello") return new Response("hi");
 *   return new Response("", { status: 404 });
 * });
 *
 * const resp = await fetch(`http://localhost:${port}/hello`);
 * expect(await resp.text()).toBe("hi");
 * await server.shutdown();
 * ```
 */
export async function mockServer(
  handler: (req: Request, url: URL) => Response | Promise<Response>,
): Promise<{ server: Deno.HttpServer; port: number }> {
  const server = Deno.serve(
    { port: 0, hostname: "127.0.0.1" },
    async (req: Request): Promise<Response> => {
      const url = new URL(req.url);

      if (req.method === "OPTIONS") {
        return new Response(null, {
          status: 204,
          headers: {
            "Access-Control-Allow-Origin": "*",
            "Access-Control-Allow-Headers": "*",
            "Access-Control-Allow-Methods": "GET, HEAD, POST, PROPFIND, OPTIONS",
          },
        });
      }

      const response = await Promise.resolve(handler(req, url));
      response.headers.set("Access-Control-Allow-Origin", "*");
      response.headers.set("Access-Control-Allow-Headers", "*");
      response.headers.set(
        "Access-Control-Allow-Methods",
        "GET, HEAD, POST, PROPFIND, OPTIONS",
      );
      return response;
    },
  );

  const port = (server.addr as Deno.NetAddr).port;
  return { server, port };
}
