interface Facet {
  url: string;
  title: string;
  category: string;
  desc: string;
}

export const layout = "layouts/kitchen-category.vto";

export default function* ({ facets }: { facets: Facet[] }) {
  const categories = [...new Set(facets.map((f) => f.category))]
    .sort() as string[];

  for (const category of categories) {
    const slug = category.toLowerCase().replace(/\s+/g, "-");
    yield {
      url: `/${slug}/`,
      title: `${category} | Diffuse`,
      category,
      slug,
      categoryFacets: facets.filter((f) => f.category === category),
    };
  }
}
