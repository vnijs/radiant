// code by Joe Cheng - https://github.com/jcheng5/shiny-resume/blob/master/www/session.js
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

  history.replaceState(null, null, search);
})
