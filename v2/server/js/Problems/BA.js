
$(document).ready(function() {
console.log( "BA.js ready!" );  
Global.Problems.ba = {



  onLoadedProblemData : function( data ){ },


  onGo : function(){
    
    $('#special').html( 
      'Text...'+
      '<br>' +
      '<canvas id="ba-canvas" width="100" height="'+ (Global.numgens * Global.numruns) +'"></canvas>' );

    this.solutions     = [] ;
    this.i             = 0  ;

    this.ctx = $('#ba-canvas')[0].getContext('2d');

  },


  fenotyp : function( solution , ffVal ){

    this.plotIt(solution , this.i );

    this.i ++ ;

  },

  plotIt : function( solution , i ){

    for( var j in solution ){
      this.ctx.fillStyle =  solution[j] ? 'white' : 'black' ;
      this.ctx.fillRect( j, i, 1, 1 );    
    }

  },





};});
