function checkifrunning() {
      var is_running = $("html").attr("class").includes("shiny-busy");
      if (is_running){
        $("#loading").show();
      } else {
        $("#loading").hide();
      }
    } 
    
function move_navpills(){
  $('.nav-pills').hide();
  $('.nav-pills > li').prependTo('.navbar-custom-menu > .navbar-nav');
  $('div > .well').remove();
  /*$('body > div.wrapper > header > nav > div > ul > li:nth-child(2) > a').
  clone().appendTo('div #go_to_catalog').wrapInner('<button type="button" class="btn btn-success btn-lg" id="btn_to_catalog"></button>');*/
}
    
setInterval(checkifrunning, 50);

setInterval(move_navpills, 50);