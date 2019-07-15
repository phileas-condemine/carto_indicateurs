
$(document).on('mouseover', '.selectize-dropdown div.option', function () {
        Shiny.setInputValue('last_tag_hovered',$(this).data('value'));
        console.log($(this).data('value'));
                         });
