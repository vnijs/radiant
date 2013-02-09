var navbarBinding = new Shiny.InputBinding();
$.extend(navbarBinding, {
  unique: 0,
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
    // $(el).on("click.navbarBinding", "a[data-view]", function(e) {
    $(el).on("click.navbarBinding", "a[data-value]", function(e) {
      if (e.target.id) {
        $(el).data("navbarBinding-lastClick", {
          link: $(e.target).data("view"),
          unique: self.unique++
        });
        e.preventDefault();
        callback();
      }
    });
  },
  unsubscribe: function(el) {
    $(el).off(".navbarBinding");
  }
});
 
Shiny.inputBindings.register(navbarBinding, "navbarBinding");