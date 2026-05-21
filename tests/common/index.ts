import { type EvaluateFunction, launch } from "@astral/astral";

export async function testWeb<T>(evalFn: EvaluateFunction<T, []>): Promise<T> {
  const url = "http://localhost:3000/testing/index.html";

  await using browser = await launch();
  await using page = await browser.newPage(url, { coverage: true });

  return await page.evaluate(evalFn);
}
