$(document).ready(function() {
  
  var t = setInterval(function(){
    Highcharts.charts.map(function(e) { e.reflow() });
    
  }, 250);
  
});