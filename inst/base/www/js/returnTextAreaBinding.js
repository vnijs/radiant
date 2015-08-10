// based on https://gist.github.com/xiaodaigh/7150112
var returnTextAreaBinding = new Shiny.InputBinding();
$.extend(returnTextAreaBinding, {

    find: function(scope) {
        return $(scope).find('.returnTextArea');
    },
    getId: function(el) {
        return $(el).attr('id')
    },
    getValue: function(el) {
        return el.value;
    },
    setValue: function(el, value) {
        el.value = value;
    },
    subscribe: function(el, callback) {
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
Shiny.inputBindings.register(returnTextAreaBinding, 'shiny.returnTextArea');
