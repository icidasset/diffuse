//
// Browser support
// (づ￣ ³￣)づ
//
//

var supported = true;


//
// Check if each test was successful

for (var feature in Modernizr) {
  if (typeof Modernizr[feature] === "boolean" && Modernizr[feature] == false) {
    supported = false;
    break;
  }
}


//
// If the browser is not supported ...

if (!supported) {
  document
    .getElementById("elm-container")
    .innerHTML = "<div class=\"Shell\"><p style=\"color: white;\">" +
                 "<strong>Sadly, your browser is not supported ☹</strong>" +
                 "<br /><small>Upgrade to a newer or different version.</small>" +
                 "</p></div>";

  window.stop();
}
