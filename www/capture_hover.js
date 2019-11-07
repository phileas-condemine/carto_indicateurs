
$(document).on('mouseover', '#tag_div div.option', function () {
        Shiny.setInputValue('last_tag_hovered',$(this).data('value'));
        console.log($(this).data('value'));
                         });
