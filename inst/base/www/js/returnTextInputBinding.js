// based on https://gist.github.com/xiaodaigh/7150112
var returnTextInputBinding = new Shiny.InputBinding();
$.extend(returnTextInputBinding, {
  find: function(scope) {
    return $(scope).find('input[type="text"]');
  },
  getId: function(el) {
    return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
  },
  getValue: function(el) {
    return el.value;
  },
  setValue: function(el, value) {
    el.value = value;
  },
  subscribe: function(el, callback) {
    // old setup worked but does not 'fire' when input is emptied
    // $(el).on('change.textInputBinding', function(event) {
    //   callback(false);
    // });

    // same setup as returnTextAreaBinding.js
    // callback when if enter key is pressed: http://stackoverflow.com/a/30149302/1974918
    $(el).on('keydown.textInputBinding input.textInputBinding', function(event) {
        if(event.keyCode == 13) {
          event.preventDefault();
          callback();
        }
        // print value using console.log(event.target.value);
        if(event.target.value == "") {
          callback();
        }
    });

    // callback when updateTextInput is used to reset value to ""
    $(el).on('change.textInputBinding', function(event) {
      if(event.target.value == "") {
        callback();
      } else {
        callback(false);
      }
    });
  },
  unsubscribe: function(el) {
    $(el).off('.textInputBinding');
  },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);

    if (data.hasOwnProperty('label'))
      $(el).parent().find('label[for=' + el.id + ']').text(data.label);

    $(el).trigger('change');
  },
  getState: function(el) {
    return {
      label: $(el).parent().find('label[for=' + el.id + ']').text(),
      value: el.value
    };
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  }
});
Shiny.inputBindings.register(returnTextInputBinding, 'returnTextInput');
