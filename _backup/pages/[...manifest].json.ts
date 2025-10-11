import type { APIRoute } from "astro";
import { getCollection } from "astro:content";

// API Route
export const GET: APIRoute = ({ params, props, request }) => {
  return new Response(JSON.stringify(props.manifest));
};

// Generate static paths
export async function getStaticPaths() {
  const manifests = await getCollection("manifests");

  return manifests.map((manifest) => {
    return {
      params: { manifest: manifest.id.replace("/_manifest", "/manifest") },
      props: { manifest: manifest.data },
    };
  });
}
