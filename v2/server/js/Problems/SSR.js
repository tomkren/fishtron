var sin  = Math.sin ;
var cos  = Math.cos ;
var exp  = Math.exp ;
var rdiv = function(x,y){if(y==0){return 1;}else{return x/y;}}
var rlog = function(x){return Math.log(Math.abs(x));}


var ssr = function( solution ){

  var trueSolution = function(x){return x*x*x*x+x*x*x+x*x+x ;}

  var xs = [ ] ; 
  for (var i = -1 ; i <= 1 ; i+= 0.01 ) {
    xs.push(i);
  }

  ysSol     = _.map( xs , function(x){return [x,    solution(x)];} );
  ysTrueSol = _.map( xs , function(x){return [x,trueSolution(x)];} );

  
  var ssrPlotData = {
    trueSolution : ysTrueSol,
    solution     : ysSol
  };

  $('#special').html(
    'Best individual : ' +
    '<div id="graphssr-div" class="graph"></div>');

  drawPlot( 'ssr' , ssrPlotData );


  return ssrPlotData;

};