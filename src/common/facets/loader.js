export function removeLoader() {
  const loader = document.querySelector("#diffuse-loader");

  if (loader) {
    loader.classList.add("loaded");
    setTimeout(() => {
      loader.remove();
      loader.parentElement?.remove();
    }, 750);
  }
}
