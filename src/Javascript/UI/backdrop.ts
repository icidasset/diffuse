// 🚀


export function init(app) {
  app.ports.pickAverageBackgroundColor.subscribe((src: string) => {
    const avgColor = pickAverageBackgroundColor(src)
    if (avgColor) app.ports.setAverageBackgroundColor.send(avgColor)
  })
}



// 🛠️


function averageColorOfImage(img: HTMLImageElement): { r: number, g: number, b: number } | null {
  const canvas = document.createElement("canvas")
  const ctx = canvas.getContext("2d")
  canvas.width = img.naturalWidth
  canvas.height = img.naturalHeight

  if (!ctx) return null

  ctx.drawImage(img, 0, 0)

  const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height)
  const color = { r: 0, g: 0, b: 0 }

  for (let i = 0, l = imageData.data.length; i < l; i += 4) {
    color.r += imageData.data[i]
    color.g += imageData.data[i + 1]
    color.b += imageData.data[i + 2]
  }

  color.r = Math.floor(color.r / (imageData.data.length / 4))
  color.g = Math.floor(color.g / (imageData.data.length / 4))
  color.b = Math.floor(color.b / (imageData.data.length / 4))

  return color
}


function pickAverageBackgroundColor(src: string): { r: number, g: number, b: number } | null {
  const img = document.querySelector(`img[src$="${src}"]`)
  return img ? averageColorOfImage(img as HTMLImageElement) : null
}
