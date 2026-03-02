const systemDark = window.matchMedia("(prefers-color-scheme: dark)");

/**
 * @param {MediaQueryListEvent | MediaQueryList} mql
 */
function applyDark(mql) {
  const isDark = mql.matches;
  document.documentElement.classList.toggle("wa-dark", isDark);
}

systemDark.addEventListener("change", applyDark);

applyDark(systemDark);
