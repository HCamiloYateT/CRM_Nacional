// Controlbar ----
(function () {
  "use strict";

  if (window.__racafeControlbarEvents) return;
  window.__racafeControlbarEvents = true;

  // Solo ventanas emergentes conocidas; no contenedores genéricos ni IDs de aplicaciones.
  const popups = [
    ".modal", ".modal-backdrop", ".daterangepicker", ".bootstrap-datetimepicker-widget",
    ".datepicker", ".datepicker-dropdown", ".flatpickr-calendar", ".ui-datepicker",
    ".clockpicker-popover", ".select2-container--open", ".swal2-container",
    ".air-datepicker", ".bs-container", ".rf-select-menu"
  ].join(", ");
  const toggles = '[data-widget="control-sidebar"], #controlbar-toggle, #controlbarPin';
  const owners = new WeakMap();
  const visible = new WeakMap();
  let origin = null;

  function open(panel) {
    return panel && panel.isConnected && document.body &&
      document.body.classList.contains("control-sidebar-slide-open") && panel.getClientRects().length > 0;
  }

  function pathOf(event) {
    return (event.composedPath ? event.composedPath() : [event.target])
      .filter(function (node) { return node instanceof Element; });
  }

  function panelOf(path) {
    for (const node of path) {
      const panel = node.closest("aside.control-sidebar");
      if (panel) return panel;
    }
    return null;
  }

  function popupOf(path) {
    for (const node of path) {
      const popup = node.closest(popups);
      if (popup) return popup;
    }
    return null;
  }

  // Los portales del picker identifican el select original. aria-controls/owns también
  // permiten reconocer menús nativos sin depender de namespaces concretos.
  function referencedPanel(popup) {
    const selectId = popup.dataset.rfSelectId;
    const select = selectId && document.getElementById(selectId);
    if (select) return select.closest("aside.control-sidebar");
    const ids = new Set([popup.id].concat(Array.from(popup.querySelectorAll("[id]"), node => node.id)));
    ids.delete("");
    if (!ids.size) return null;
    for (const control of document.querySelectorAll("[aria-controls], [aria-owns]")) {
      const refs = (control.getAttribute("aria-controls") || "") + " " + (control.getAttribute("aria-owns") || "");
      if (refs.split(/\s+/).some(id => ids.has(id))) {
        return control.closest("aside.control-sidebar");
      }
    }
    return null;
  }

  // Registra la procedencia al abrirse, no al hacer clic en cualquier modal de la página.
  function remember(popup) {
    const shown = popup.matches(popups) && popup.isConnected && popup.getClientRects().length > 0 &&
      getComputedStyle(popup).visibility !== "hidden";
    const wasShown = visible.get(popup);
    visible.set(popup, shown);
    if (!shown) { owners.delete(popup); return; }
    if (wasShown) return;
    const panel = referencedPanel(popup) || origin;
    if (open(panel)) owners.set(popup, panel);
  }

  function discover(node) {
    if (!(node instanceof Element)) return;
    const popup = node.closest(popups);
    if (popup) remember(popup);
    node.querySelectorAll(popups).forEach(remember);
  }

  function observe() {
    if (!document.body) return;
    // Los emergentes que ya estaban abiertos no se atribuyen a un clic posterior del panel.
    document.querySelectorAll(popups).forEach(remember);
    new MutationObserver(function (records) {
      for (const record of records) {
        if (record.type === "attributes") {
          // No recorrer de nuevo el documento al cambiar una clase de body o de un input.
          if (record.target.matches(popups) || visible.has(record.target)) remember(record.target);
        } else {
          record.removedNodes.forEach(function (node) {
            if (!(node instanceof Element)) return;
            const removed = node.matches(popups) ? [node] : [];
            removed.push.apply(removed, node.querySelectorAll(popups));
            removed.forEach(function (popup) { owners.delete(popup); visible.delete(popup); });
          });
          record.addedNodes.forEach(discover);
        }
      }
    }).observe(document.body, { childList: true, subtree: true, attributes: true,
      attributeFilter: ["class", "style", "aria-hidden"] });
  }

  document.addEventListener("focusin", function (event) {
    const path = pathOf(event), panel = panelOf(path);
    if (open(panel)) { origin = panel; return; }
    const popup = popupOf(path);
    if (!popup) origin = null;
  });

  // La captura solo registra la procedencia antes de que un plugin abra el popup.
  // No detiene ni cancela eventos; evita atribuir un modal ajeno al último panel enfocado.
  document.addEventListener("click", function (event) {
    if (!document.body || !document.body.classList.contains("control-sidebar-slide-open")) return;
    const path = pathOf(event);
    if (path.some(node => node.matches(toggles))) { origin = null; return; }
    const panel = panelOf(path), popup = popupOf(path);
    const owner = popup && (referencedPanel(popup) || owners.get(popup));
    origin = open(panel) ? panel : open(owner) ? owner : null;
  }, true);

  document.addEventListener("click", function (event) {
    const target = event.target;
    if (!(target instanceof Element) || !document.body ||
        !document.body.classList.contains("control-sidebar-slide-open")) return;
    const path = pathOf(event);
    // Conserva el cierre/fijación nativos incluso si el botón pierde su icono durante el clic.
    if (path.some(node => node.matches(toggles))) { origin = null; return; }
    const panel = panelOf(path);
    if (open(panel)) {
      origin = panel;
      event.stopPropagation();
      return;
    }
    const popup = popupOf(path);
    const owner = popup && (referencedPanel(popup) || owners.get(popup));
    if (open(owner)) {
      origin = owner;
      // Fase bubble en document: los handlers de Shiny/Bootstrap en document se ejecutan.
      // No se llama a preventDefault ni stopImmediatePropagation.
      event.stopPropagation();
    } else origin = null;
  }, false);

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", observe, { once: true });
  else observe();
})();
