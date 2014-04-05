
$(function() {

var problemObject ;

var problemData;

var linksDiv  ;
var flySim    ;

var solutions ; 
var nextSolutionID ;



var log = function(x){ return console.log(x); };

//---------------------------------------------------------------------

var onLoadedProblemData = function( data ){ 
  //log(data);
  problemData = data; 

};

var onGo = function(){

  solutions      = [] ;
  nextSolutionID = 0  ;


  var container = $('#special');

  var infoText = '<b>Each number</b> (=fitness) <b>link represents '+
                 'one best-of-generation individual. Click it and see!</b>';
      
  linksDiv = $('<div>');
  var flySimContainer = $('<div>').attr('id','_fly-sim');

  container
   .html('')
   .append( infoText )
   .append( linksDiv )
   .append( '<hr>' ) 
   .append( flySimContainer ); 

  flySim = FlySim.init( '_fly-sim' , function(sim){

    sim.loadLevelsDB( problemData.Levels ); //Levels );
    
    sim.loadLevel( 'w0' , 'my-prog4' , FlySim.progLib.prog4 );

    sim.loadSolution( 'my-prog1' , FlySim.progLib.prog1 );

    problemObject.Funs = sim.Funs ; 


    //sim.run();

  });  
  

};

var fenotyp = function( solution , ffVal , actGen ){ 

  var currSolutionID = nextSolutionID ;
  nextSolutionID ++ ;

  solutions[ currSolutionID ] = solution ;

  if( actGen == 0 ){
    linksDiv.append('<hr>');  
  }

  var newLink = $('<a>')
   //.attr('href','#')
   .css('cursor','pointer')
   .attr('title', solution.toString() )
   .html( ffVal )
   .click( function(){ onLinkClick(currSolutionID); } );

  linksDiv.append( newLink ).append(' ');

};

// ------------------------------------------

var onLinkClick = function( solutionID ){
  log( solutions[ solutionID ].toString() );
  //haxxx = solutions[ solutionID ] ;

  flySim.addNewSolution( 'Best of gen '+solutionID , solutions[ solutionID ] );
  flySim.play();

};

var mkProblemObject = function(){
  return {
    Funs                : {}, // inicializuje se až v onGo()
    onLoadedProblemData : onLoadedProblemData,
    onGo                : onGo,
    fenotyp             : fenotyp,

    getProblemData      : function(){ return problemData; },
    getFlySim           : function(){ return flySim; }
  };
};

problemObject = mkProblemObject(); 
Global.Problems.fly02 = problemObject;
log( "Fly02.js ready!" ); 

});

