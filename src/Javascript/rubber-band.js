//
// Rubber Band
// \ (•◡•) /
//
// This prevents the iOS rubber-band scrolling on the body element.
// It's based on the https://github.com/lazd/iNoBounce library,
// with the difference that this'll scroll if the app is > 100vh.


let touchStartY = 0,
    supportsPassiveOption = false


try {
  window.addEventListener("test", null, Object.defineProperty(
    {}, "passive", { get: _ => { supportsPassiveOption = true } }
  ))
} catch (e) {
  //
}


const documentTouchStart = event => {
	touchStartY = event.touches ? event.touches[0].screenY : event.screenY
}


const documentTouchMove = event => {
	let el = event.target

	while (el !== document.body && el !== document) {
		const style = window.getComputedStyle(el)
		if (!style) break

		// Ignore range input element
		if (el.nodeName === "INPUT" && el.getAttribute("type") === "range") return

    // Determine if the element should scroll
		const scrolling = style.getPropertyValue("-webkit-overflow-scrolling")
		const overflowY = style.getPropertyValue("overflow-y")
		const height = parseInt(style.getPropertyValue("height"), 10)
		const isScrollable = scrolling === "touch" && (overflowY === "auto" || overflowY === "scroll")
		const canScroll = el.scrollHeight > el.offsetHeight

		if (isScrollable && canScroll) {
			const currentY = event.touches ? event.touches[0].screenY : event.screenY
			const isAtTop = (touchStartY <= currentY && el.scrollTop === 0)
			const isAtBottom = (touchStartY >= currentY && el.scrollHeight - el.scrollTop === height)
			if (isAtTop || isAtBottom) event.preventDefault()
			return
		}

		el = el.parentNode
	}

  if (document.body.scrollHeight <= window.innerHeight) {
    event.preventDefault()
  }
}


window.addEventListener(
  "touchstart",
  documentTouchStart,
  supportsPassiveOption ? { passive : false } : false
)


window.addEventListener(
  "touchmove",
  documentTouchMove,
  supportsPassiveOption ? { passive : false } : false
)
