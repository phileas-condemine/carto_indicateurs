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

function fix_boxes_height(){
  if($(window).width() >= 768){
    max_height = Math.max($("#placeholder_datatable .box_text_content").height())+20;
    $("#placeholder_datatable .box").height(max_height);
  } else {
    $("#placeholder_datatable .box").each(function() {
      $(this).height($(this).children().innerHeight());
    });
  }
}

function reveal_iframe(){
  if($(".modal-dialog iframe").length){
    if($(".modal-dialog iframe")[0].style.height=="0px"){
      $(".modal-dialog iframe").height("300px");
      $(".modal-dialog iframe").width("100%");
    }
    if($(".modal-dialog iframe").width()<110){
      console.log($(".slick-slide")[0].style.width);
      console.log($(".modal-dialog iframe").width());
      /*$('.modal-dialog iframe').width("100%");*/
      $('.slick-dots a').first().click();
      $(".modal-dialog iframe").css('width','100%');
    }
  }
}




setInterval(fix_boxes_height, 100);

setInterval(reveal_iframe, 500);

setInterval(checkifrunning, 50);

setInterval(move_navpills, 50);

setInterval(check_input_length, 500);


