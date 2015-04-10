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

  if (search.length == 0) {
    search = "?SSUID=" + encodeURIComponent(data);
  } else if (reSSUID.test(search)) {
    search = search.replace(reSSUID, "$1");
    if (!/[?&]$/.test(search))
      search += "&";
    search += "SSUID=" + encodeURIComponent(data);
  }

  // Work around ShinyApps.io/SSP/RSC base href silliness
  var path = location.pathname.replace(/\/_w_(\w+)/, '');

  history.replaceState(null, null, path + search);
})
