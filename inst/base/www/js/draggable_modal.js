// From http://stackoverflow.com/a/27122472/1974918
$(function(){
  $('.modal').modal({ keyboard: false,
                     show: true
  });
  // Jquery draggable
  $('.modal-dialog').draggable({
      handle: ".modal-header"
  });
});

// not working ... yet
// perhaps when jquery-ui.js is upgraded in the next shiny release
