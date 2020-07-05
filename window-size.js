var width = 0;
$(document).on("shiny:connected", function(e) {
  height = window.innerHeight;
  width = window.innerWidth;
  Shiny.onInputChange("height", height);
  Shiny.onInputChange("width", width);
});