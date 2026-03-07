interface Facet {
  url: string;
  title: string;
  category: string;
  desc: string;
}

export const layout = "layouts/facets-category.vto";

export default function* ({ facets }: { facets: Facet[] }) {
  const categories = [...new Set(facets.map((f) => f.category))].sort() as string[];

  for (const category of categories) {
    const slug = category.toLowerCase().replace(/\s+/g, "-");
    yield {
      url: `/facets/${slug}/`,
      title: `${category} | Facets | Diffuse`,
      category,
      slug,
      categoryFacets: facets.filter((f) => f.category === category),
    };
  }
}
