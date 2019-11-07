$(document).on('shiny:busy', function(event) {
  var agent = navigator.userAgent;
  console.log(agent);
  Shiny.onInputChange("check", agent);
});
