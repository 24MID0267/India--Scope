// www/indiascope.js
// Client-side interactions for IndiaScope

$(document).ready(function() {
  // Listen for Escape key to reset back to India view
  $(document).keydown(function(e) {
    if (e.key === "Escape") {
      var resetBtn = $("#map_ui-btn_reset_india");
      if (resetBtn.length > 0) {
        resetBtn.click();
      }
    }
  });

  // Ensure Leaflet map invalidates size on window resize or tab switch
  window.addEventListener('resize', function() {
    var mapEl = document.querySelector('.leaflet-container');
    if (mapEl && window.HTMLWidgets) {
      window.HTMLWidgets.resize();
    }
  });
});
