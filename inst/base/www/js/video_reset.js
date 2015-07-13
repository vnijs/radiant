// from http://stackoverflow.com/a/31078774/1974918
$(function(){
  $("body").on('hidden.bs.modal', function (e) {
    var $iframes = $(e.target).find("iframe");
    $iframes.each(function(index, iframe){
      $(iframe).attr("src", $(iframe).attr("src"));
    });
  });
});

// from http://stackoverflow.com/a/28114558/1974918
// needed to wrap in funct(){}
$(function(){
	$('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
	  var $iframe = $(e.relatedTarget.hash).find('iframe');
	  $iframe.attr("src", $iframe.attr("src"));
	});
});
