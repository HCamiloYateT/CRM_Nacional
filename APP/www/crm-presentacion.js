(function ($) {
  "use strict";
  $(function () {
    if (!$.fn.tooltip) return;
    // El foco tras un clic no debe mantener abierto el hover compartido.
    $(document.body).tooltip("dispose").tooltip({
      selector: "[title], [data-original-title]", placement: "right", container: "body",
      trigger: "hover", animation: false
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
    window.addEventListener("scroll", hideTooltips, true);
  });
}(window.jQuery));
