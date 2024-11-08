// custom.js

shinyjs.init = function() {
  var originalOnVisibilityChange = document.onvisibilitychange;
  document.onvisibilitychange = function() {
    var hidden = document.hidden;
    if (!hidden) {
      Shiny.setInputValue('appActive', true);
    }
    if (originalOnVisibilityChange) {
      originalOnVisibilityChange.apply(document, arguments);
    }
  };
};
shinyjs.init();
