/*
 * collabnet-topbar.js -- D2-B2: icon-only top bar with accessible names and tooltips (Bootstrap 3, CSP-safe).
 *
 * - Every top-bar control gets an accessible name (aria-label from its title) and a BS3 tooltip shown on hover AND
 *   keyboard focus (trigger "hover focus"). bslib's tooltip() does the same by making the trigger focusable
 *   (bslib srcts/src/components/webcomponents/tooltip.ts:99-102, MIT); this is our own BS3 implementation.
 * - <label> wrappers around hidden action inputs (.cnet download, API keys) become focusable role=button controls;
 *   Enter/Space click the hidden input.
 * - Touch: a long press (>= 500 ms) shows the tooltip and swallows the following click; a normal tap acts as usual.
 * - D3: the D2 hamburger menu is retired; the bar wraps (heading row, then an icon row) at every width (CSS).
 * - D3: tooltips open BELOW the bar (BS3 placement "auto bottom", viewport-bounded) so they are never cut off by the
 *   top of the window; bslib's tooltip() defaults to placement "auto" for the same reason (bslib R/tooltip.R:92, MIT).
 * - D3: any visible label text on a top-bar control (e.g. after updateActionButton, or controls inserted later by
 *   renderUI/insertUI/JS) is moved into a BS3 .sr-only span, so the bar stays icon-only but screen readers keep it.
 * No inline handlers; loaded as an external 'self' script.
 */
(function (root) {
  "use strict";
  var LONG_PRESS_MS = 500;

  function labelOf(el) {
    return (el.getAttribute("aria-label") || el.getAttribute("title") || el.getAttribute("data-original-title") || "").trim();
  }

  // Pure helper (unit-tested): the short visible label used in the small-screen menu.
  function shortLabel(title) {
    var t = String(title || "").trim();
    var cut = t.search(/[:(,.]| - | — /);
    t = (cut > 0 ? t.slice(0, cut) : t).trim();
    return t.length > 40 ? t.slice(0, 39) + "…" : t;
  }

  var TOOLTIP_OPTS = { trigger: "hover focus", placement: "auto bottom", container: "body", viewport: { selector: "body", padding: 8 } };

  // Move visible label text into .sr-only so the control shows only its icon (pure DOM; unit-tested with a fake DOM).
  function hideVisibleText(el) {
    var doc = el.ownerDocument, kids = Array.prototype.slice.call(el.childNodes || []);
    kids.forEach(function (n) {
      if (n.nodeType === 3 && n.nodeValue && n.nodeValue.trim()) {
        var sp = doc.createElement("span"); sp.className = "sr-only"; sp.textContent = n.nodeValue.trim();
        el.replaceChild(sp, n);
      }
    });
    var labels = el.querySelectorAll ? el.querySelectorAll(".action-label, .cn-btn-label") : [];
    for (var i = 0; i < labels.length; i++) {
      var c = labels[i].classList;
      if (c && !c.contains("sr-only") && (labels[i].textContent || "").trim()) c.add("sr-only");
    }
  }

  function enhanceControl(el, $) {
    hideVisibleText(el);
    if (el.__cnTopbar) return;
    el.__cnTopbar = true;
    var name = labelOf(el);
    if (name && !el.getAttribute("aria-label")) el.setAttribute("aria-label", name);
    if (name && !el.getAttribute("data-cn-label")) el.setAttribute("data-cn-label", shortLabel(name));
    if (el.tagName === "LABEL") {
      var input = el.querySelector("input.action-button, input[type=button]");
      el.setAttribute("role", "button");
      el.setAttribute("tabindex", "0");
      el.addEventListener("keydown", function (ev) {
        if (ev.key === "Enter" || ev.key === " ") { ev.preventDefault(); if (input) input.click(); else el.click(); }
      });
    }
    if ($ && $.fn && $.fn.tooltip) {
      var j = $(el);
      if (j.data && j.data("bs.tooltip")) j.tooltip("destroy");   // a page-wide init may have run first with placement top
      $(el).tooltip(TOOLTIP_OPTS);
    }
    var timer = null, longPressed = false;
    el.addEventListener("touchstart", function () {
      longPressed = false;
      timer = setTimeout(function () { longPressed = true; if ($ && $.fn && $.fn.tooltip) $(el).tooltip("show"); }, LONG_PRESS_MS);
    }, { passive: true });
    var cancel = function () { if (timer) { clearTimeout(timer); timer = null; } };
    el.addEventListener("touchend", cancel);
    el.addEventListener("touchmove", cancel, { passive: true });
    el.__cnLongPressed = function () { var v = longPressed; longPressed = false; return v; };
  }

  function attach(win) {
    var doc = win.document, $ = win.jQuery;
    var bar = doc.querySelector(".header-container");
    var buttons = bar && bar.querySelector(".header-buttons");
    if (!bar || !buttons) return null;
    var controls = buttons.querySelectorAll('[data-toggle="tooltip"]');
    for (var i = 0; i < controls.length; i++) enhanceControl(controls[i], $);
    // a long press must not also trigger the action (capture phase runs before Shiny's jQuery click handler)
    doc.addEventListener("click", function (ev) {
      var c = ev.target && ev.target.closest && ev.target.closest(".header-buttons [data-toggle]");
      if (c && c.__cnLongPressed && c.__cnLongPressed()) { ev.preventDefault(); ev.stopPropagation(); }
    }, true);
    // controls added or relabelled later (renderUI / insertUI / updateActionButton / JS) are enhanced too
    if (win.MutationObserver) new win.MutationObserver(function () {
      var cs = buttons.querySelectorAll('[data-toggle="tooltip"], .btn');
      for (var k = 0; k < cs.length; k++) enhanceControl(cs[k], $);
    }).observe(buttons, { childList: true, subtree: true, characterData: true });
    // theme toggle: keep its accessible name / tooltip in sync with the body.dark-mode class (server toggles it)
    var theme = doc.getElementById("theme_toggle");
    var syncTheme = function () {
      if (theme) setTitle(theme, themeTitle(doc.body.classList.contains("dark-mode")), $);
    };
    syncTheme();
    if (win.MutationObserver) new win.MutationObserver(syncTheme).observe(doc.body, { attributes: true, attributeFilter: ["class"] });
    return { controls: controls.length };
  }

  function themeTitle(dark) {
    return dark ? "Switch to Light Mode (currently dark)" : "Switch to Dark Mode (currently light)";
  }

  function setTitle(el, text, $) {
    el.setAttribute("aria-label", text);
    el.setAttribute("data-cn-label", shortLabel(text));
    if (el.getAttribute("data-original-title") !== null) el.setAttribute("data-original-title", text); // BS3 stores it here
    else el.setAttribute("title", text);
    if ($ && $.fn && $.fn.tooltip && $(el).data("bs.tooltip")) $(el).tooltip("fixTitle");
  }

  var api = { hideVisibleText: hideVisibleText, TOOLTIP_OPTS: TOOLTIP_OPTS, shortLabel: shortLabel, themeTitle: themeTitle, setTitle: setTitle, enhanceControl: enhanceControl, attach: attach, LONG_PRESS_MS: LONG_PRESS_MS };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else {
    root.CollabNETTopbar = api;
    var go = function () { root.__cnTopbar = attach(root); };
    if (root.document.readyState === "loading") root.document.addEventListener("DOMContentLoaded", go); else go();
  }
})(typeof window !== "undefined" ? window : this);
