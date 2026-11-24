// C25_r10: empty-state spotlight (all editions). body.cn-spotlight is on while the empty-state note (#cn_no_data_note,
// shown by the conditionalPanel on output.cn_has_data) is visible, i.e. no dataset is loaded. An overlay dims the page
// (css/collabnet-spotlight.css); the top bar and the Fetch card stay above it. The overlay holds no focusable element
// and does not move focus (no focus trap). Clicking it or pressing Esc dismisses the spotlight; it comes back the next
// time the app returns to the empty state (data cleared). Browser only, no dependencies.
(function () {
  "use strict";
  var dismissed = false, wasEmpty = null;
  function isEmpty() {
    var n = document.getElementById("cn_no_data_note");
    return !!(n && n.getClientRects().length);   // hidden conditionalPanel -> display:none -> no client rects
  }
  function update() {
    var empty = isEmpty(), b = document.body;
    if (!b) return;
    if (!empty) dismissed = false;   // data loaded: the next empty state shows the spotlight again
    if (empty && wasEmpty === false) dismissed = false;
    wasEmpty = empty;
    b.classList.toggle("cn-spotlight", empty && !dismissed);
  }
  function dismiss() { dismissed = true; update(); }
  function reset() { dismissed = false; update(); }   // C25_r11: Close / reset shows the spotlight again
  function init() {
    var ov = document.getElementById("cn-spotlight-overlay");
    if (!ov) {
      ov = document.createElement("div"); ov.id = "cn-spotlight-overlay"; ov.setAttribute("aria-hidden", "true");
      document.body.appendChild(ov);
    }
    ov.addEventListener("click", dismiss);
    document.addEventListener("keydown", function (e) { if (e.key === "Escape" && document.body.classList.contains("cn-spotlight")) dismiss(); });
    var note = document.getElementById("cn_no_data_note"), panel = note && note.parentElement;
    if (panel && window.MutationObserver) new MutationObserver(update).observe(panel, { attributes: true, attributeFilter: ["style", "class"] });
    if (window.jQuery) window.jQuery(document).on("shiny:value shiny:conditional shiny:connected", function () { setTimeout(update, 0); });
    update();
  }
  window.CollabnetSpotlight = { update: update, dismiss: dismiss, reset: reset, isEmpty: isEmpty };
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", init); else init();
})();
