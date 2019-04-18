shinyjs.move_navpills = function(){
/*$('.nav-pills > li').prependTo('.navbar-custom-menu > .navbar-nav');
$('div > .well').remove();*/
$('body > div.wrapper > header > nav > div > ul > li:nth-child(2) > a').
clone().appendTo('div #go_to_catalog').wrapInner('<button type="button" class="btn btn-success btn-lg" id="btn_to_catalog"></button>');
  
};