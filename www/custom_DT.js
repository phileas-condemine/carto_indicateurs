function(settings, json) {
  /*$.when($('#filter_in_sidebar').remove()).then(function(){*/
  $('.dataTables_filter').addClass("hide");
  /*insertAfter('#logo_in_sidebar').
  prependTo('.treeview-menu').
  wrap('<div id="filter_in_sidebar" class="form-group shiny-input-container"></div>');
  });*/
  $('input[type=search]').
  addClass('selectize-input').
  attr("placeholder", "Filtre textuel");
  
  /*$('#filter_in_sidebar:not(:last)').remove();*/
  
  /*function setTextContents($elem, text) {
    $elem.contents().filter(function() {
      if (this.nodeType == Node.TEXT_NODE) {
        this.nodeValue = text;
      }
    });
  }
  setTextContents($('.dataTables_filter label'), 'Recherche par mot(s) clef(s)');*/
}
