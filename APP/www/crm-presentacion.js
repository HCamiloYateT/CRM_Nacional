(function ($) {
  "use strict";
  $(function () {
    if (!$.fn.tooltip) return;
    // El foco tras un clic no debe mantener abierto el hover compartido.
    $(document.body).tooltip("dispose").tooltip({
      selector: "[title]:not([data-crm-select-tip]), [data-original-title]:not([data-crm-select-tip])",
      placement: "auto", container: "body",
      trigger: "hover", animation: false
    });
    // Mostrar el componente corporativo antes del envío, sin temporizadores que oculten trabajo todavía pendiente.
    var loaderSequence = 0, loaderInputs = Object.create(null);
    $(document).on("shiny:inputchanged.crmLoader", function (event) {
      var el = event.el || document.getElementById(event.name.split(":")[0]);
      if (!el || !window.Shiny || !Shiny.shinyapp || !Shiny.shinyapp.isConnected()) return;
      if (!el.closest(".rf-form,.rf-field,.form-group,.tabbable,.main-sidebar,.control-sidebar")) return;
      var signature = JSON.stringify(event.value);
      if (Object.prototype.hasOwnProperty.call(loaderInputs, event.name) &&
          loaderInputs[event.name] === signature) return;
      loaderInputs[event.name] = signature;
      var trigger = document.getElementById("crm_recalcular");
      if (trigger) {
        trigger.click();
        // Garantiza un ciclo confirmado por R incluso para acciones que solo abren UI en el cliente.
        Shiny.setInputValue("crm_recalcular", ++loaderSequence, {priority: "deferred"});
      }
    });
    // Texto accesible y hover para enlaces cuyo texto se oculta al colapsar el sidebar.
    function syncSidebarTitles() {
      var collapsed = document.body.classList.contains("sidebar-collapse");
      $(".main-sidebar .nav-sidebar a.nav-link").each(function () {
        var link = $(this), text = link.children("p").first().clone();
        text.find(".right, .badge, .nav-icon").remove();
        var label = text.text().replace(/\s+/g, " ").trim();
        if (!label) return;
        link.attr("aria-label", label);
        if (collapsed) {
          link.attr("title", label).attr("data-crm-sidebar-hover", "true");
        } else if (link.attr("data-crm-sidebar-hover")) {
          if (link.data("bs.tooltip")) link.tooltip("dispose");
          link.removeAttr("title data-original-title data-crm-sidebar-hover");
        }
      });
    }
    syncSidebarTitles();
    new MutationObserver(syncSidebarTitles).observe(document.body, {attributes: true, attributeFilter: ["class"]});
    var sidebar = document.querySelector(".main-sidebar");
    if (sidebar) new MutationObserver(syncSidebarTitles).observe(sidebar, {childList: true, subtree: true});
    $(document).on("shiny:value.crmSidebar shown.lte.pushmenu collapsed.lte.pushmenu", syncSidebarTitles);

    $(document).on("click", "[data-crm-crear-tarea]", function () {
      document.body.classList.add("control-sidebar-slide-open");
      var link = $(".control-sidebar .nav-link").filter(function () {
        return $(this).text().toLowerCase().indexOf("crear tareas") >= 0;
      }).first();
      if (link.length) link.tab("show");
    });
    var selectTip, selectOwner, selectTimer;
    function hideSelectTip() {
      clearTimeout(selectTimer);
      if (selectOwner) {
        var described = (selectOwner.getAttribute("aria-describedby") || "").split(/\s+/)
          .filter(function (id) { return id && id !== "crm-select-tooltip"; });
        if (described.length) selectOwner.setAttribute("aria-describedby", described.join(" "));
        else selectOwner.removeAttribute("aria-describedby");
      }
      if (selectTip) selectTip.remove();
      selectTip = selectOwner = null;
    }
    function showSelectTip(button) {
      hideSelectTip();
      if (button.disabled || button.classList.contains("disabled") ||
          button.classList.contains("dropdown-active") || button.getAttribute("aria-expanded") === "true") return;
      var field = button.closest(".rf-field,.form-group"), select = field && field.querySelector("select");
      if (!select) return;
      var label = field.querySelector("label"), values = Array.from(select.selectedOptions)
        .map(function (option) { return option.textContent.trim(); }).filter(Boolean);
      var text = (label ? label.textContent.trim() + ": " : "") +
        (values.length ? values.join(" · ") : "Ninguna opción seleccionada");
      var tip = document.createElement("div");
      tip.id = "crm-select-tooltip"; tip.className = "crm-select-tooltip";
      tip.setAttribute("role", "tooltip"); tip.textContent = text; document.body.appendChild(tip);
      var r = button.getBoundingClientRect(), box = tip.getBoundingClientRect(), gap = 10, edge = 12;
      var above = r.top - gap - edge, below = window.innerHeight - r.bottom - gap - edge;
      var top = above >= box.height || above >= below;
      var available = Math.max(0, top ? above : below);
      if (available < 24) { tip.remove(); return; }
      tip.style.maxHeight = available + "px"; box = tip.getBoundingClientRect();
      tip.style.left = Math.max(edge, Math.min(r.left + (r.width - box.width) / 2,
        window.innerWidth - box.width - edge)) + "px";
      tip.style.top = (top ? r.top - gap - box.height : r.bottom + gap) + "px";
      selectTip = tip; selectOwner = button;
      var described = (button.getAttribute("aria-describedby") || "").trim();
      button.setAttribute("aria-describedby", (described ? described + " " : "") + tip.id);
    }
    function syncSelectTips() {
      document.querySelectorAll('.bootstrap-select > button,' +
        '.selectize-control > .selectize-input,.form-group > select:not(.selectized):not(.selectpicker)').forEach(function (b) {
        if ($(b).data("bs.tooltip")) $(b).tooltip("dispose");
        b.removeAttribute("title"); b.removeAttribute("data-original-title");
        b.setAttribute("data-crm-select-tip", "true");
      });
    }
    syncSelectTips();
    var tipsFrame;
    new MutationObserver(function () {
      cancelAnimationFrame(tipsFrame); tipsFrame = requestAnimationFrame(syncSelectTips);
      if (selectOwner && !selectOwner.isConnected) hideSelectTip();
    }).observe(document.body, {childList: true, subtree: true});
    $(document).on("mouseenter.crmSelect focusin.crmSelect", "[data-crm-select-tip]", function () {
      var button = this; clearTimeout(selectTimer);
      selectTimer = setTimeout(function () { showSelectTip(button); }, 180);
    }).on("mouseleave.crmSelect focusout.crmSelect", "[data-crm-select-tip]", hideSelectTip);
    $(document).on("mousedown.crmSelect shown.bs.select hide.bs.modal shiny:disconnected", hideSelectTip);
    $(document).on("keydown.crmSelect", "[data-crm-select-tip]", hideSelectTip);
    window.addEventListener("resize", hideSelectTip);
    // Texto del selector actualizado: el próximo hover consulta selectedOptions de nuevo.
    $(document).on("changed.bs.select", function () { hideSelectTip(); syncSelectTips(); });
    function hideTooltips() {
      $("[aria-describedby]").each(function () {
        if ($(this).data("bs.tooltip")) $(this).tooltip("hide");
      });
    }
    $(document).on("click.crmTips hide.bs.modal shiny:disconnected", hideTooltips);
    $(document).on("shiny:value", function () {
      $(".tooltip").each(function () {
        var tip = this;
        var owners = $("[aria-describedby]").filter(function () {
          return (this.getAttribute("aria-describedby") || "").split(/\s+/).indexOf(tip.id) >= 0;
        });
        if (!owners.length) $(tip).remove();
      });
    });
    window.addEventListener("scroll", function () { hideTooltips(); hideSelectTip(); }, true);
  });
}(window.jQuery));

