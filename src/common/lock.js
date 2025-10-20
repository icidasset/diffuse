/**
 * @returns {PromiseWithResolvers<void> & { status: PromiseWithResolvers<"acquired" | "waiting"> }}
 */
export function lock() {
  const w = Promise.withResolvers();

  return {
    ...w,
    status: Promise.withResolvers(),
  };
}
