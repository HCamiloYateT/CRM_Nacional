(function ($) {
  "use strict";
  $(function () {
    if (!$.fn.tooltip) return;
    // El foco tras un clic no debe mantener abierto el hover compartido.
    $(document.body).tooltip("dispose").tooltip({
      selector: "[title], [data-original-title]", placement: "right", container: "body",
      trigger: "hover", animation: false
    });
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
