// return a parameter value from the current URL
function getParam ( sname ) {
  var params = location.search.substr(location.search.indexOf("?")+1);
  var sval = "";
  params = params.split("&");
    // split param and value into individual pieces
    sval = 'dataview';
    for (var i=0; i<params.length; i++)
       {
         temp = params[i].split("=");
         if ( [temp[0]] == sname ) { sval = temp[1]; }
       }
  return sval;
}

var toolBinding = new Shiny.InputBinding();
$.extend(toolBinding, {
  find: function(scope) {
    return $(scope).find(".tool");
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, value) {
    $(el).val(value);
  },
  subscribe: function(el, callback) {
  	$(el).val(getParam("tool"));
  	$(el).trigger("change");
  	callback();
  },
  unsubscribe: function(el) {
    $(el).off(".toolBinding");
  }
});

Shiny.inputBindings.register(toolBinding);
