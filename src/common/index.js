/**
 * @template T
 * @param {Array<T>} array
 * @returns Array<T>
 */
export function arrayShuffle(array) {
  if (array.length === 0) {
    return [];
  }

  array = [...array];

  for (let index = array.length - 1; index > 0; index--) {
    const randArr = crypto.getRandomValues(new Uint32Array(1));
    const randVal = randArr[0] / 2 ** 32;
    const newIndex = Math.floor(randVal * (index + 1));
    [array[index], array[newIndex]] = [array[newIndex], array[index]];
  }

  return array;
}
