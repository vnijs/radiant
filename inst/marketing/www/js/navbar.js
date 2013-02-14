var navbarBinding = new Shiny.InputBinding();
$.extend(navbarBinding, {
  find: function(scope) {
    return $(scope).find(".navbar");
  },
  getValue: function(el) {
    return $(el).data("navbarBinding-lastClick");
  },
  setValue: function(el, value) {
    $(el).text(value);
  },
  subscribe: function(el, callback) {
    var self = this;
    $(el).on("click.navbarBinding", "a[data-value]", function(e) {
      $(el).data("navbarBinding-lastClick", $(e.target).data("value"));
      e.preventDefault();
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".navbarBinding");
  }
});
 
Shiny.inputBindings.register(navbarBinding, "navbarBinding");
