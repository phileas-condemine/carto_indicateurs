{
    option: function(item, escape) {
      return "<div><p style='text-align: left'><strong>" + escape(item.label) + "</strong> <span style='float:right;'>(" +
             "freq: " + escape(item.value) + ")</span></p></div>";
    }
}

