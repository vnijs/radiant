var navbarBinding = new Shiny.InputBinding();

$.extend(navbarBinding, {

  find: function(scope) {
    return $(scope).find(".navbar");
  },
  getValue: function(el) {
    return $(el).data("nav-value");
  },
  setValue: function(el, value) {
    $(el).text(value);
  },
  subscribe: function(el, callback) {
    $(el).on("click.navbarBinding", "a[data-value]", function(e) {
      $(el).data("nav-value", $(e.target).data("value"));
      $('.dropdown.open').removeClass('open');
      callback();
    });

  },
  unsubscribe: function(el) {
    $(el).off(".navbarBinding");
  }
});
 
Shiny.inputBindings.register(navbarBinding, "navbarBinding");
