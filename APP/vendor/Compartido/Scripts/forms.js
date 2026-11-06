(function ($) {
  "use strict";
  if ($.data(document, "rfSharedForms")) return;
  $.data(document, "rfSharedForms", true);
  var initial = new WeakMap();
  var registered = false;
  var reset_id = 0;
  var supported = "input:not([type='file']):not([type='hidden']),textarea,select," +
    ".shiny-date-range-input,.shiny-input-radiogroup,.shiny-input-checkboxgroup";

  function remember(input) {
    if (!input.closest(".rf-form[id]") || !input.matches(supported) || initial.has(input)) return;
    var binding = $(input).data("shiny-input-binding");
    if (!binding || !binding.getValue) return;
    try {
      initial.set(input, { binding: binding, value: JSON.parse(JSON.stringify(binding.getValue(input))) });
    } catch (error) {
      // Un control no compatible se omite; nunca registrar sus valores ni mensajes técnicos.
    }
  }

  $(document).on("shiny:bound.rfSharedForms", ".rf-form[id] .shiny-bound-input", function () { remember(this); });
  $(function () { $(".rf-form[id] .shiny-bound-input").each(function () { remember(this); }); });

  $(document).on("click.rfSharedForms keydown.rfSharedForms", ".rf-card[data-rf-action]", function (event) {
    if (event.type === "keydown" && !["Enter", " "].includes(event.key)) return;
    if (event.type === "keydown") event.preventDefault();
    if (!window.Shiny) return;
    var count = Number($.data(this, "rfActionCount") || 0) + 1;
    $.data(this, "rfActionCount", count);
    Shiny.setInputValue(this.getAttribute("data-rf-action"), count, { priority: "event" });
  });

  async function reset(params) {
    var form = document.getElementById(params.id);
    var result = { success: true, count: 0, failures: 0, request_id: ++reset_id };
    if (!form || !form.matches(".rf-form")) result.success = false;
    else {
      var inputs = Array.from(form.querySelectorAll(".shiny-bound-input"));
      for (var input of inputs) {
        var stored = initial.get(input);
        if (!stored || input.readOnly) continue;
        try {
          var value = stored.value;
          var message;
          if (input.matches(".sw-air-picker")) {
            message = params.clear ? { clear: true, config: {} } :
              { config: { value: { value: value ? value.date : [], tz: value ? value.tz : undefined } } };
          } else if (input.matches(".shiny-date-range-input")) {
            message = { value: { start: params.clear ? null : value[0], end: params.clear ? null : value[1] } };
          } else {
            if (params.clear) {
              if (input.matches("input[type='checkbox']")) value = false;
              else if (input.multiple || input.matches(".shiny-input-checkboxgroup")) value = [];
              else value = "";
            }
            message = { value: value };
          }
          await stored.binding.receiveMessage(input, message);
          result.count += 1;
        } catch (error) {
          result.failures += 1;
          result.success = false;
        }
      }
    }
    if (window.Shiny) Shiny.setInputValue(params.id + "_reset_result", result, { priority: "event" });
  }

  function register() {
    if (registered || !window.Shiny || !Shiny.addCustomMessageHandler) return;
    registered = true;
    Shiny.addCustomMessageHandler("rf-form-reset", reset);
  }
  $(register);
  $(document).on("shiny:connected.rfSharedForms", register);
}(window.jQuery));
