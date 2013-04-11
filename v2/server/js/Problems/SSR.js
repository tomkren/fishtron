
$(document).ready(function() {
console.log( "SSR.js ready!" );  
Global.Problems.ssr = {



  onLoadedProblemData : function( data ){ },


  onGo : function(){
    
    $('#special').html( 
      '<b>Each number</b> (=fitness) <b>link represents one best-of-generation individual. Click it and see!</b>'+
      '<div id="ssr-links"></div><br>' +
      '<div id="graphssr-div" class="graph"></div>' );

    this.solutions     = [] ;
    this.i             = 0  ;

  },


  fenotyp : function( solution , ffVal ){

    ffVal = Math.round(ffVal*100)/100 ;

    this.plotIt(solution);

    this.solutions[this.i] = solution ;
    $('#ssr-links').append( '<a href="#" title="'+ solution +'" '+
      'onclick="Global.Problems.ssr.ssrLink('+this.i+')">' +ffVal + '</a> ' );

    this.i ++ ;

  },

  plotIt : function( solution ){

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

    drawPlot( 'ssr' , ssrPlotData );

  },


  ssrLink : function( i ){
    
    this.plotIt( this.solutions[i] );

  },



};});
