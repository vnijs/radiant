// setInterval(function(){
//   if ($('html').attr('class')=='shiny-busy') {
//     setTimeout(function() {
//       if ($('html').attr('class')=='shiny-busy') {
//         $('div.busy').show()
//       }
//     }, 1000)
    
//   } else {
//     $('div.busy').hide()
//   }
// },100)

setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    $('div.busy').show()
  } else {
    $('div.busy').hide()
  }
},10)