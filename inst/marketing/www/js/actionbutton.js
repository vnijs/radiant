jQuery(function($) {
  var actionButtonBinding = new Shiny.InputBinding();
  $.extend(actionButtonBinding, {
    find: function(scope) {
      return $(scope).find(".action-button");
    },
    getValue: function(el) {
      var val = $(el).data('val') || 0;
      $(el).data('val', val + 1);
      return val;
    },
    setValue: function(el, value) {
    },
    subscribe: function(el, callback) {
      $(el).on("click.actionButton", function(e) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off(".actionButton");
    }
  });
  Shiny.inputBindings.register(actionButtonBinding);
})