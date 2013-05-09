
$(document).ready(function() {

console.log( "Ant.js ready!" );  

var antState; 


Global.Problems.ant = {


  onLoadedProblemData : function( data ){ 
    this.antState.loadResources();
  },


  onGo : function(){
    
    $('#special').html( 
      '<b>Each number</b> (=fitness) <b>link represents one best-of-generation individual. Click it and see!</b>'+
      '<div id="ant-links"></div><br>' +
      '<canvas id="antCanvas" width="517" height="517"></canvas>' );

    this.solutions     = [] ;
    this.i             = 0  ;

    this.antState.init();

  },


  fenotyp : function( solution , ffVal ){

    this.solutions[this.i] = solution ;
    $('#ant-links').append( '<a href="#" title="'+ solution +'" '+
      'onclick="Global.Problems.ant.link('+this.i+')">' +ffVal + '</a> ' );

    this.i ++ ;

  },




  link : function( i ){

    this.antState.add( this.solutions[i]() );

  },

  antState : {
    pos : undefined,
    dir : undefined ,
    world : [],
    limit : 600,
    steps : undefined,
    eaten : undefined,
    ctx : undefined,
    antImg : {},
    seedImg : undefined,
    isRunning : false,
    progQueue : [],
    actAnt : 0,
    initWorldStrs :   
     [ ".FFF............................"
     , "...F............................"
     , "...F.....................FFF...." 
     , "...F....................F....F.." 
     , "...F....................F....F.." 
     , "...FFFF.FFFFF........FF........." 
     , "............F................F.." 
     , "............F.......F..........." 
     , "............F.......F..........." 
     , "............F.......F........F.." 
     , "....................F..........." 
     , "............F..................." 
     , "............F................F.." 
     , "............F.......F..........." 
     , "............F.......F.....FFF..." 
     , ".................F.....F........" 
     , "................................" 
     , "............F..................."             
     , "............F...F.......F......."
     , "............F...F..........F...." 
     , "............F...F..............."             
     , "............F...F..............."
     , "............F.............F....." 
     , "............F..........F........" 
     , "...FF..FFFFF....F..............." 
     , ".F..............F..............." 
     , ".F..............F..............." 
     , ".F......FFFFFFF................." 
     , ".F.....F........................" 
     , ".......F........................" 
     , "..FFFF.........................." 
     , "................................" ],

    loadResources : function(){

      var dirs = [ 'u' , 'd' , 'l' , 'r' ];

      for( var i in dirs ){
        antState.antImg[ dirs[i] ] = new Image();  
        antState.antImg[ dirs[i] ].src = 'img/ant-'+ dirs[i] +'.png';      
      }
      antState.seedImg = new Image();
      antState.seedImg.src = 'img/weed.png' ;

    },

    init : function(){

      antState.steps = 0;
      antState.eaten = 0;
      antState.dir   = 'r'; 
      antState.pos   = [0,0] ;  

      for( var i in antState.initWorldStrs ){
        var lineStr = antState.initWorldStrs[i] ;
        antState.world[i] = [];
        for( var j in lineStr ){
          antState.world[i][j] = (lineStr[j] == 'F') ;
        }
      }

      antState.drawInit();

      antState.draw();
    },

    drawInit : function(){

      if( ! $('#antCanvas').length ){
        $('#special').html(
           '<canvas id="antCanvas" width="517" height="517"></canvas>'+
           '<div id="antLogs" style="position:relative;top:-520px;left:520px;"></div>'
         );
        var canvas = document.getElementById('antCanvas');
        antState.ctx = canvas.getContext('2d');
      } else {
        antState.ctx = $('#antCanvas')[0].getContext('2d');
      }
    },

    draw : function(){

      antState.ctx.clearRect(0, 0, 16*32 +5, 16*32 +5);

      antState.drawAnt();

      for( var i in antState.world ){
        for( var j in antState.world[i] ){
          if( antState.world[i][j] ){
            antState.drawSeed(i,j);
          }
        }
      }

    },

    drawAnt : function(){
      antState.ctx.drawImage( antState.antImg[ antState.dir ] , antState.pos[1]*16 , antState.pos[0]*16 );
    },

    drawSeed : function(i,j){
      antState.ctx.drawImage( antState.seedImg , j*16 - 4 , i*16 );
    },

    show : function(){
      var ret = '\n' ;
      for( var i in antState.world ){
        for( var j in antState.world[i] ){
          if( i == antState.pos[0] && j == antState.pos[1] ){
            ret = ret + antState.dir ;
          }else{
            ret = ret + (antState.world[i][j] ? 'F' : '.') ; 
          }
        }
        ret = ret + '\n' ;
      }

      ret = ret + 'eaten : ' + antState.eaten + '\n' ;
      ret = ret + 'steps : ' + antState.steps ;

      return ret ;
    },

    ahead : function(){
      var p0 = antState.pos[0];
      var p1 = antState.pos[1];
      switch( antState.dir ){
        case 'r' : p1 = (p1+1+32) % 32 ; break;
        case 'l' : p1 = (p1-1+32) % 32 ; break;
        case 'u' : p0 = (p0-1+32) % 32 ; break;
        case 'd' : p0 = (p0+1+32) % 32 ; break;
      }
      return [p0,p1] ;
    },  

    reachedLimit : function(){
      return antState.steps >= antState.limit ;
    },

    doMove : function(){
      if( antState.reachedLimit() ){ return; }

      var ah = antState.ahead();
      antState.pos[0] = ah[0];
      antState.pos[1] = ah[1];

      if( antState.world[ah[0]][ah[1]] ){
        antState.eaten ++ ;
        antState.world[ah[0]][ah[1]] = false ;
      }

      antState.steps ++ ;
      antState.draw();
    },

    doLeft : function(){

      if( antState.reachedLimit() ){ return; }

      switch( antState.dir ){
        case 'r' : antState.dir = 'u' ; break;
        case 'l' : antState.dir = 'd' ; break;
        case 'u' : antState.dir = 'l' ; break;
        case 'd' : antState.dir = 'r' ; break;
      }

      antState.steps ++ ; 
      antState.draw();   
    },

    doRight : function(){
      if( antState.reachedLimit() ){ return; }

      switch( antState.dir ){
        case 'r' : antState.dir = 'd' ; break;
        case 'l' : antState.dir = 'u' ; break;
        case 'u' : antState.dir = 'r' ; break;
        case 'd' : antState.dir = 'l' ; break;
      } 

      antState.steps ++ ;  
      antState.draw(); 
    },

    isFoodAhead : function(){
      var ah = antState.ahead( );
      return antState.world[ah[0]][ah[1]];
    },

    run : function( prog ){

      //console.log(prog);
      //haxx = prog;

      var stepoid = function(){
        if( ! antState.reachedLimit() ){
          prog();
          setTimeout( stepoid , 50 );
        }else{
          antState.isRunning = false ;
          if( antState.progQueue.length ){
            antState.start( antState.progQueue.shift() );
          }
        }
      };

      stepoid();

    },

    start : function( prog ){
        antState.isRunning = true;
        antState.init();
        var antI = ++ antState.actAnt ;
        antState.run( prog );
    },

    add : function( prog ){



      if( antState.isRunning ){

        antState.progQueue.push( prog );
      } else {
        antState.start( prog );
      }

    },


  },



};

antState = Global.Problems.ant.antState; // kvuli tomu, že v antState se furt odkazuje na promenou 
                                         // antState... (v puvodnim umisteni kodu to nebyla takova prasarna)

// HAX (!) schvalne tu nejsou var před nima

l   = antState.doLeft  ;
r   = antState.doRight ;
m   = antState.doMove  ;
p2  = function( a1 , a2 ){ return function(){ a1() ; a2() ; } }
p3  = function(a1,a2,a3 ){ return function(){ a1() ; a2() ; a3() ;} }
ifa = function( a1 , a2 ){ return function(){if( antState.isFoodAhead() ){ a1(); }else{ a2(); } } }




});

//var l   ;
//var r   ;
//var m   ;
//var p2  ;
//var p3  ;
//var ifa ;
