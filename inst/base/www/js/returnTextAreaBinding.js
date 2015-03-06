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
        // $(el).on('keyup.textInputBinding input.textInputBinding', function(event) {
        $(el).on('keydown.textInputBinding input.textInputBinding', function(event) {
            if(event.keyCode == 13 && (event.metaKey || event.ctrlKey)) { //if enter key is pressed
            // if(event.keyCode == 13 && event.ctrlKey) { //if enter key is pressed
                callback()
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
