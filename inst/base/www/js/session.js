Shiny.addCustomMessageHandler("session_start", function(data) {
  var search = location.search;

  // ""
  // "?"
  // "?SSUID=x"
  // "?SSUID=x&foo=bar"
  // "?foo=bar&SSUID=x"
  // "?foo=bar&SSUID=x&baz=quux"
  // "?foo=bar"
  // "?foo=bar&baz=quux"
  var reSSUID = /([?&])SSUID=[^&]*&?/g;

  // if (search.length == 0) {
  //   search = "?SSUID=" + encodeURIComponent(data);
  // } else if (reSSUID.test(search)) {
  //   search = search.replace(reSSUID, "$1");
  //   if (!/[?&]$/.test(search))
  //     search += "&";
  //   search += "SSUID=" + encodeURIComponent(data);
  // } else {
  //   if (!/[?&]$/.test(search))
  //     search += "&";
  //   search += "SSUID=" + encodeURIComponent(data);
  // }

  if (search.length > 0) {
    if (reSSUID.test(search))
      search = search.replace(reSSUID, "$1");
    if (!/[?&]$/.test(search))
      search += "&";
    search += "SSUID=" + encodeURIComponent(data);
  } else {
    search = "?SSUID=" + encodeURIComponent(data);
  }

  // Work around ShinyApps.io/SSP/RSC base href silliness
  // var path = location.pathname.replace(/\/_w_(\w+)/, '');

  // Use window.top to accomodate iframes on shinyapps.io
  // https://groups.google.com/d/msg/shinyapps-users/YrLV52e_InY/K11pYWxZ8G8J
  // var path = window.top.location.pathname.replace(/\/_w_(\w+)/, "");
  // window.top.history.replaceState(null, null, path + search);

  // Joe Cheng: "Work around ShinyApps.io/SSP/RSC base href silliness"
  var path = location.pathname.replace(/\/_w_(\w+)/, "");
  history.replaceState(null, null, path + search);

  // prior version
  // history.replaceState(null, null, search);
})

// Not working as intended
// Shiny.addCustomMessageHandler("new_session", function(data) {
//   var path = location.pathname.replace(/\/.+/, '');
//   history.replaceState(null, null, path);
// })

// for firefox and chrome? Doesn't seem to work
// function window_close() {
//   window.open("","_parent","");
//   window.close();
// }
