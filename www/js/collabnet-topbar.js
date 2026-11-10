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
 * - D4: the BS3 tooltips are gone from the top bar. Each control shows its label INSIDE the button (icon + text pill)
 *   on hover / :focus-visible / long-press (collabnet-topbar-labels.css). Late label text is moved into the single
 *   .cn-topbar-label span (normalizeLabel) instead of being re-hidden. Pill direction (right, or left near the
 *   viewport edge) is chosen on mouseenter/focusin. Pattern idea: bslib toolbar icon/label buttons
 *   (bslib inst/components/scss/toolbar.scss:83, MIT); our own implementation.
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

  // D4: no Bootstrap tooltips on top-bar controls any more -- the label expands inside the button instead
  // (collabnet-topbar-labels.css). One markup pattern: <control aria-label=...><icon/><span class="cn-topbar-label">Label</span>.
  var LABEL = "cn-topbar-label", OPEN_MS = 2500, EDGE = 8;

  // Pure DOM (unit-tested with a fake DOM): make sure the control has exactly one .cn-topbar-label holding its current
  // label. Late text (updateActionButton, renderUI/insertUI, JS) is MOVED into the label instead of being hidden;
  // legacy D2/D3 ".cn-btn-label sr-only" spans are converted. Returns true if the DOM was changed.
  function normalizeLabel(el) {
    var doc = el.ownerDocument, changed = false;
    var legacy = el.querySelectorAll ? el.querySelectorAll(".cn-btn-label") : [];
    for (var i = 0; i < legacy.length; i++) {
      legacy[i].className = LABEL; changed = true;
    }
    var holders = [el];
    var al = el.querySelector ? el.querySelector(".action-label") : null;
    if (al) holders.push(al);
    var stray = [];
    holders.forEach(function (h) {
      Array.prototype.slice.call(h.childNodes || []).forEach(function (n) {
        if (n.nodeType === 3 && n.nodeValue && n.nodeValue.trim()) stray.push(n);
      });
    });
    var labels = el.querySelectorAll ? el.querySelectorAll("." + LABEL) : [];
    var label = labels[0] || null;
    for (var k = 1; k < labels.length; k++) { labels[k].parentNode.removeChild(labels[k]); changed = true; }
    if (stray.length) {
      var text = stray.map(function (n) { return n.nodeValue.trim(); }).join(" ");
      stray.forEach(function (n) { n.parentNode.removeChild(n); });
      if (!label) { label = doc.createElement("span"); label.className = LABEL; (al || el).appendChild(label); }
      label.textContent = text; changed = true;
    }
    if (!label) {
      var name = shortLabel(labelOf(el));
      if (name) { label = doc.createElement("span"); label.className = LABEL; label.textContent = name; el.appendChild(label); changed = true; }
    }
    if (label && label.classList && label.classList.contains("sr-only")) { label.classList.remove("sr-only"); changed = true; }
    if (label && !el.getAttribute("aria-label") && (label.textContent || "").trim()) {
      el.setAttribute("aria-label", label.textContent.trim()); changed = true;
    }
    return changed;
  }

  // Pure (unit-tested): which way the pill opens so it stays on screen.
  function expandDirection(btnLeft, btnRight, pillWidth, viewportWidth) {
    var roomRight = viewportWidth - EDGE - btnLeft, roomLeft = btnRight - EDGE;
    if (pillWidth <= roomRight) return "right";
    if (pillWidth <= roomLeft) return "left";
    return roomLeft > roomRight ? "left" : "right";
  }

  function setDirection(el, win) {
    var label = el.querySelector("." + LABEL);
    if (!label || !el.getBoundingClientRect) return;
    var r = el.getBoundingClientRect(), w = label.scrollWidth || label.getBoundingClientRect().width;
    var dir = expandDirection(r.left, r.right, Math.max(w, r.width), win.innerWidth || 0);
    if (el.getAttribute("data-cn-expand") !== dir) el.setAttribute("data-cn-expand", dir);
    // E1: equal padding in the open pill. The text-side padding mirrors the icon's inset from the button edge.
    var pad = iconInset(el, r, dir);
    if (pad != null && el.style && el.style.setProperty) el.style.setProperty("--cn-pill-pad", pad + "px");
  }

  // Inset (px, minus the pill's 1px border) of the visible icon group (all children except the label / sr-only
  // text) from the button's outer edge on the side the pill grows away from. null if nothing measurable.
  function iconInset(el, r, dir) {
    var kids = el.children || [], l = Infinity, rr = -Infinity;
    for (var i = 0; i < kids.length; i++) {
      var k = kids[i], cl = k.classList;
      if (!k.getBoundingClientRect || (cl && (cl.contains(LABEL) || cl.contains("sr-only")))) continue;
      var q = k.getBoundingClientRect(); if (!q.width) continue;
      l = Math.min(l, q.left); rr = Math.max(rr, q.right);
    }
    if (l === Infinity) return null;
    var inset = dir === "left" ? r.right - rr : l - r.left;
    return Math.max(4, Math.round((inset - 1) * 2) / 2);
  }

  function enhanceControl(el, $, win) {
    if (!el.__cnTopbar) {   // a long title becomes the accessible name before the short visible label is derived
      var t0 = el.getAttribute("title") || el.getAttribute("data-original-title");
      if (t0 && !el.getAttribute("aria-label")) el.setAttribute("aria-label", t0.trim());
    }
    normalizeLabel(el);
    if (el.__cnTopbar) return;
    el.__cnTopbar = true;
    // remove the D2/D3 tooltip and the native title bubble: they would duplicate the inline label
    if ($ && $.fn && $.fn.tooltip && $(el).data && $(el).data("bs.tooltip")) $(el).tooltip("destroy");
    ["title", "data-original-title", "data-toggle", "data-placement"].forEach(function (a) { el.removeAttribute(a); });
    var name = labelOf(el);
    if (name && !el.getAttribute("data-cn-label")) el.setAttribute("data-cn-label", shortLabel(name));
    if (el.tagName === "LABEL") {
      var input = el.querySelector("input.action-button, input[type=button]");
      el.setAttribute("role", "button");
      el.setAttribute("tabindex", "0");
      el.addEventListener("keydown", function (ev) {
        if (ev.key === "Enter" || ev.key === " ") { ev.preventDefault(); if (input) input.click(); else el.click(); }
      });
    }
    if (win) {
      var dir = function () { setDirection(el, win); };
      el.addEventListener("mouseenter", dir);
      el.addEventListener("focusin", dir);
    }
    // touch: a long press (>= 500 ms) expands the label in place for OPEN_MS and swallows the following click
    var timer = null, longPressed = false, closeT = null;
    el.addEventListener("touchstart", function () {
      longPressed = false;
      timer = setTimeout(function () {
        longPressed = true;
        if (win) setDirection(el, win);
        el.classList.add("cn-tb-open");
        clearTimeout(closeT); closeT = setTimeout(function () { el.classList.remove("cn-tb-open"); }, OPEN_MS);
      }, LONG_PRESS_MS);
    }, { passive: true });
    var cancel = function () { if (timer) { clearTimeout(timer); timer = null; } };
    el.addEventListener("touchend", cancel);
    el.addEventListener("touchmove", cancel, { passive: true });
    el.__cnLongPressed = function () { var v = longPressed; longPressed = false; return v; };
  }

  function controlsOf(buttons) {
    return Array.prototype.filter.call(buttons.children, function (c) { return c.id !== "cn-topbar-toggle"; });
  }

  function attach(win) {
    var doc = win.document, $ = win.jQuery;
    var bar = doc.querySelector(".header-container");
    var buttons = bar && bar.querySelector(".header-buttons");
    if (!bar || !buttons) return null;
    var enhanceAll = function () { controlsOf(buttons).forEach(function (c) { enhanceControl(c, $, win); }); };
    enhanceAll();
    // a long press must not also trigger the action (capture phase runs before Shiny's jQuery click handler)
    doc.addEventListener("click", function (ev) {
      var c = ev.target && ev.target.closest && ev.target.closest(".header-buttons > *");
      if (c && c.__cnLongPressed && c.__cnLongPressed()) { ev.preventDefault(); ev.stopPropagation(); }
    }, true);
    // a touch elsewhere closes an open label
    doc.addEventListener("touchstart", function (ev) {
      var open = buttons.querySelectorAll(".cn-tb-open");
      for (var i = 0; i < open.length; i++) if (!open[i].contains(ev.target)) open[i].classList.remove("cn-tb-open");
    }, { passive: true });
    // controls added or relabelled later (renderUI / insertUI / updateActionButton / JS) get the same pattern;
    // normalizeLabel is idempotent, so our own edits settle after one extra observer pass
    if (win.MutationObserver) new win.MutationObserver(enhanceAll).observe(buttons, { childList: true, subtree: true, characterData: true });
    // theme toggle: keep its accessible name in sync with the body.dark-mode class (server toggles it)
    var theme = doc.getElementById("theme_toggle");
    var syncTheme = function () {
      if (theme) setTitle(theme, themeTitle(doc.body.classList.contains("dark-mode")));
    };
    syncTheme();
    if (win.MutationObserver) new win.MutationObserver(syncTheme).observe(doc.body, { attributes: true, attributeFilter: ["class"] });
    return { controls: controlsOf(buttons).length };
  }

  function themeTitle(dark) {
    return dark ? "Switch to Light Mode (currently dark)" : "Switch to Dark Mode (currently light)";
  }

  function setTitle(el, text) {
    if (el.getAttribute("aria-label") !== text) el.setAttribute("aria-label", text);
    if (el.getAttribute("data-cn-label") !== shortLabel(text)) el.setAttribute("data-cn-label", shortLabel(text));
  }

  var api = { normalizeLabel: normalizeLabel, expandDirection: expandDirection, iconInset: iconInset, setDirection: setDirection, LABEL: LABEL, shortLabel: shortLabel, themeTitle: themeTitle, setTitle: setTitle, enhanceControl: enhanceControl, attach: attach, LONG_PRESS_MS: LONG_PRESS_MS };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else {
    root.CollabNETTopbar = api;
    var go = function () { root.__cnTopbar = attach(root); };
    if (root.document.readyState === "loading") root.document.addEventListener("DOMContentLoaded", go); else go();
  }
})(typeof window !== "undefined" ? window : this);
