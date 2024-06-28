export function failure(text: string): void {
  const note = document.createElement("div")

  note.className = "flex flex-col font-body items-center h-screen italic justify-center leading-relaxed px-4 text-center text-base text-white"
  note.innerHTML = `
    <a class="block logo mb-5" href="../">
      <img src="../images/diffuse-light.svg" />
    </a>

    <p class="max-w-sm opacity-60">
      ${text}
    </p>
  `

  document.body.appendChild(note)

  // Remove loader
  const elm = document.querySelector("#elm")
  elm?.parentNode?.removeChild(elm)
}
