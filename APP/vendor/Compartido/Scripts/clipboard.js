(function ($) {
  "use strict";
  if ($.data(document, "rfSharedClipboard")) return;
  $.data(document, "rfSharedClipboard", true);
  var request_id = 0;
  var registered = false;

  function feedback(success, type) {
    var text = success ? (type === "image" ? "Imagen copiada." : "Texto copiado.") :
      "No se pudo copiar. Revise los permisos del portapapeles y use HTTPS o localhost.";
    if (window.Shiny && Shiny.notifications) {
      Shiny.notifications.show({ html: text, type: success ? "message" : "error", duration: 4000 });
    }
  }

  async function copy(params, type) {
    var result = { success: false, type: type, error: null, request_id: ++request_id };
    try {
      if (!window.isSecureContext) throw new DOMException("", "SecurityError");
      if (!navigator.clipboard) throw new DOMException("", "NotSupportedError");
      if (type === "image") {
        if (!window.ClipboardItem || !navigator.clipboard.write) throw new DOMException("", "NotSupportedError");
        var url = new URL(params.image, document.baseURI);
        if (!["http:", "https:", "data:"].includes(url.protocol) ||
          (url.protocol === "data:" && !params.image.startsWith("data:image/png;"))) {
          throw new DOMException("", "NotSupportedError");
        }
        var response = await fetch(url.href);
        if (!response.ok) throw new DOMException("", "NetworkError");
        var image = await response.blob();
        if (image.type !== "image/png") throw new DOMException("", "NotSupportedError");
        await navigator.clipboard.write([new ClipboardItem({ "image/png": image })]);
      } else {
        if (typeof params.text !== "string") throw new DOMException("", "NotSupportedError");
        await navigator.clipboard.writeText(params.text);
      }
      result.success = true;
    } catch (error) {
      var known = ["NotAllowedError", "SecurityError", "NotSupportedError", "NetworkError"];
      result.error = known.includes(error.name) ? error.name : "CopyFailed";
    }
    if (window.Shiny && params.result_id) Shiny.setInputValue(params.result_id, result, { priority: "event" });
    if (params.notify !== false) feedback(result.success, type);
  }

  $(document).on("click.rfSharedClipboard", ".rf-copy-button", function () {
    var button = this;
    var params = { result_id: button.id + "_copy_result", notify: button.dataset.rfNotify !== "false" };
    if (button.hasAttribute("data-rf-copy-image")) {
      params.image = button.getAttribute("data-rf-copy-image");
      copy(params, "image");
      return;
    }
    params.text = button.getAttribute("data-rf-copy-text");
    if (button.hasAttribute("data-rf-copy-target")) {
      var target = document.getElementById(button.getAttribute("data-rf-copy-target"));
      params.text = target ? ("value" in target ? target.value : target.textContent) : null;
    }
    copy(params, "text");
  });

  function register() {
    if (registered || !window.Shiny || !Shiny.addCustomMessageHandler) return;
    registered = true;
    Shiny.addCustomMessageHandler("rf-copy-text", function (params) { copy(params, "text"); });
    Shiny.addCustomMessageHandler("rf-copy-image", function (params) { copy(params, "image"); });
  }
  $(register);
  $(document).on("shiny:connected.rfSharedClipboard", register);
}(window.jQuery));
