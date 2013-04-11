

var dStay   = 0; 
var dUp     = 1;
var dDown   = 2;
var dLeft   = 3; 
var dRight  = 4; 

var lt = function(x,y){
  return (x<=y) ;
};

var head_ = function( xs ){
  if( xs.length == 0 ) return null;
  return xs[0];
};

var avg = function( poses ){

  if( poses.length == 0 ) return null;

  var sum0 = 0;
  var sum1 = 0;

  for( var i in poses ){
    var pos = poses[i];
    sum0 += pos[0];
    sum1 += pos[1];
  }

  return [ Math.round( sum0 / poses.length ) , Math.round( sum1 / poses.length ) ];
};

var output_ = function( dir_ ){
  return dir_ ;
};

var myApplePoses_ = function( input_ ){
  return input_[0];
};

var nearestFlyPos_ = function( input_ ){
  return input_[1];
};

var myPos_ = function( input_ ){
  return input_[2];
};

var inputEnergy_ = function( input_ ){
  return input_[3];
};


var posToDir_ = function( posMy , posHer ){ 
      
  if( posHer === null ){
    return dStay ;
  }
       
  var d = minus( posHer , posMy );
  var dx = d[0] , dy = d[1];
       
  if(   dx  > dy && (-dx) > dy ) { return dUp   ; }
  if(   dx  > dy               ) { return dRight; }
  if( (-dx) > dy               ) { return dLeft ; }
  if( true                     ) { return dDown ; }
      
};

var minus = function( pos1 , pos2 ){ 
  return [ pos1[0]-pos2[0] , pos1[1]-pos2[1] ]; 
};

var dist = function( pos1 , pos2 ) {
    var d = minus( pos1 , pos2 );
    var dx = d[0] , dy = d[1];
    return Math.sqrt(  dx*dx + dy*dy );
};


$(document).ready(function() {

console.log( "Fly.js ready!" );  





var Fly;
Global.Problems.fly = Fly = {

  progLib : {
    prog1 : function (x0){return output_(posToDir_(  myPos_(x0)   , head_(myApplePoses_(x0))  )   );},
    prog2 : function (x0){return output_(dRight);},
    prog3 : function (x0){return output_(posToDir_(  myPos_(x0)   , nearestFlyPos_(x0) )  );},
  },

//prog_1 input = output_ $ posToDir_ (myPos_ input) (head_ $ myApplePoses_ input)
//prog_2 _     = output_ $ dRight 
//prog_3 input = output_ $ posToDir_ (myPos_ input) (nearestFlyPos_ input)



  onLoadedProblemData : function( data ){
    //console.log( data );
    this.loadResources();
    this.dataJSON = JSON.stringify( data.world );
    this.loadFromDataJSON( );
  },

  restart : function( solution , drawIt ){

    if( solution === undefined ){
      solution = this.defaultSolution ;
      this.actualSolution = solution  ;
    }
    if( drawIt   === undefined ){drawIt   = true;}

    this.loadFromDataJSON( );
    this.mapaAt( this.solutionFlyPos ).prog = solution ;
    if(drawIt){ this.drawMapa(); }
  },

  loadFromDataJSON : function( ){

    var data = JSON.parse( this.dataJSON );

    this.mapa = data.mapa ;
    
    this.mapa[data.solutionFlyPos[0]][data.solutionFlyPos[1]] = {  
      type       : 'fly' ,
      progName   : '_'   ,
      energy     : 1  
    };

    this.solutionFlyPos = data.solutionFlyPos ;

    this.fliesToDo      = data.fliesToDo;
    this.doneFlies      = data.doneFlies;
    this.applePoses     = data.applePoses;
    
    this.numSteps       = data.numSteps;

    this.fliesToDo.unshift( data.solutionFlyPos );

    for( var y in this.mapa ){
      for( var x in this.mapa[y] ){
        var obj = this.mapa[x][y] ;
        if( obj.type === 'fly' ){
          obj.prog = this.progLib[ obj.progName ] ;
        }
      }
    }

  },

  loadResources : function(){

    //console.log( this );

    this.antImg = {};

    var dirs = [ 'u' , 'd' , 'l' , 'r' ];

    for( var i in dirs ){
      this.antImg[ dirs[i] ] = new Image();  
      this.antImg[ dirs[i] ].src = 'img/ant-'+ dirs[i] +'.png';      
    }

    this.flyImg = new Image();
    this.flyImg.src = 'img/fly.png' ;

    this.appleImg = new Image();
    this.appleImg.src = 'img/apple.png' ;

    this.wallImg = new Image();
    this.wallImg.src = 'img/wall.png' ;
  },

  onGo : function(){

    this.solutions     = [] ;
    this.i             = 0  ;

    $('#special').html( 
      '<b>Each number</b> (=fitness) <b>link represents one best-of-generation individual. Click it and see!</b>'+
      '<div id="fly-links"></div><br>' +
      '<table><tr><td><div id="fly-speed">Speed&nbsp;&nbsp;&nbsp;</div>' +
      '<td><div id="fly-speed-slider" style="font-size: 75%;width:600px"></div></table><br>'+
      '<table><tr><td><div id="fly-speed">Time&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</div>' +
      '<td><div id="fly-time-slider" style="font-size: 75%;width:600px"></div></table><br>'+
      '<canvas id="fly-canvas" width="666" height="666"></canvas>' +
      '<div id="fly-info-container" style="font-size:66%"></div>' );


    this.ctx = $('#fly-canvas')[0].getContext('2d');

    //$(function() {
    //  $( "#fly-speed-slider" ).slider();
    //});

    var that = this;

    this.maxSpeed  = 500 ;
    this.initSpeed = 450 ;

    $(function() {
      $( "#fly-speed-slider" ).slider({
        range: "min",
        value: that.initSpeed ,
        min: 0,
        max: that.maxSpeed,
        slide: function( event, ui ) {
          that.changeSpeed( ui.value );
        }
      });
      that.changeSpeed( $( "#fly-speed-slider" ).slider( "value" ) );
    });

    $(function() {
      $( "#fly-time-slider" ).slider({
        range: "min",
        value: 0 ,
        min: 0,
        max: that.numSteps,
        slide: function( event, ui ) {
          $( "#fly-speed-slider" ).slider( "value" , 0 );
          that.changeSpeed( 0 );
          that.goToStepN( ui.value );
        }
      });
      //that.changeSpeed( $( "#fly-time-slider" ).slider( "value" ) );
    });

    this.drawMapa( );

  },

  changeSpeed : function( newSpeed ){
    this.speed = this.maxSpeed - newSpeed;
  },

  fenotyp : function( solution , ffVal ){
    this.solutions[this.i] = solution ;
    $('#fly-links').append( '<a href="#" title="'+ solution +'" '+
      'onclick="Global.Problems.fly.flyLink('+this.i+')">' +ffVal + '</a> ' );

    if( this.i == 0 ){
      //console.log('roflik');
      this.defaultSolution = solution ;
      this.mapaAt( this.solutionFlyPos ).prog = this.defaultSolution ;
    }

    this.i ++ ;

  },

  flyLink : function( i ){
    this.actualSolution = this.solutions[i];
    //console.log('lol : ' +i+ ' ' + this.actualSolution );
    this.restart( this.actualSolution );
    this.run();
  },

  run : function( numSteps ){

    var delta = 1 ;

    if( numSteps === undefined ){
      numSteps = this.numSteps ;
    }

    if( numSteps == 0 ){ 
      return ; 
    } else {
      
      if( ! this.paused ){
        this.step();
        this.drawMapa( ) ;
        $( "#fly-time-slider" ).slider( "value" , this.numSteps - numSteps ) ;
      } else {
        delta = 0 ;
      }
      
      this.paused = this.speed == this.maxSpeed ;
      
      var that = this;
      setTimeout( function(){that.run(numSteps - delta );} , this.speed );  
    }
    
  },

  goToStepN : function( n ){
    this.restart( this.actualSolution , false );
    
    for( var i = 0 ; i < n ; i++  ){
      this.step();
    }

    this.drawMapa();
  },

  step : function(){
    if( this.fliesToDo.length == 0 ){
      this.fliesToDo = this.doneFlies.reverse();
      this.doneFlies = [];
    } else {
      this.stepCurrentFly();
      this.step();
    }
  },

  stepCurrentFly : function(){

    if( this.fliesToDo.length == 0 ){ throw "There should be a fly!" ; }

    var currentFlyPos = this.fliesToDo.shift();
    var fly           = this.mapaAt( currentFlyPos );
    var input         = this.prepareInput( currentFlyPos , fly ); 
    var output        = fly.prog( input ); 

    //console.log( fly.progName + ' ' + output );

    var newFlyPos     = this.tryToMoveFly( output , currentFlyPos ) ;
    
    this.doneFlies.unshift( newFlyPos );

  },



  prepareInput : function( flyPos, fly ){

    return [ this.getSortedPoses( flyPos , this.applePoses ) ,
             this.getNearestFlyPos( flyPos ) ,
             flyPos,
             fly.energy ];
  },


  tryToMoveFly : function( dir , pos ){

    var newPos = this.posPlusDir( pos , dir );

    switch( this.mapaAt(newPos).type ){
      case 'free'  : this.moveFromTo(pos,newPos); return newPos ;  
      case 'apple' : this.eatApple(pos,newPos)  ; return newPos ; 
      default      : /* nothing.. */              return pos    ; 
    }


  },


  moveFromTo : function( from , to ){
    var obj = this.mapaAt( from );
    this.deleteOnPos( from );
    this.putObjOnPos( to , obj )
  },

  // eatApple :: Pos -> Pos -> World -> World
  // eatApple flyPos applePos w0 = 
  //   let w1 = moveFromTo flyPos applePos w0
  //       w2 = updateEnergy (+1) applePos w1
  //    in w2 { applePoses = delete applePos (applePoses w2) }

  eatApple : function( flyPos , applePos ){
    this.moveFromTo( flyPos , applePos );
    this.mapaAt(applePos).energy ++;
    this.deleteFromPoses( this.applePoses , applePos );
    
  },

  deleteOnPos : function( pos ){
    this.mapa[pos[0]][pos[1]] = { type : 'free' };
  },

  deleteFromPoses : function( poses , what ){
    var index = undefined;
    for( var i in poses ){
      if( poses[i][0] == what[0] && poses[i][1] == what[1] ){
        index = i;
        break;
      }
    }

    if( index !== undefined ){
      poses.splice(index, 1);
    }
  },

  putObjOnPos : function( pos , obj ){
    this.mapa[pos[0]][pos[1]] = obj;
  },

  posPlusDir : function( pos , dir ){
    var x = pos[0];
    var y = pos[1];

    switch( dir ){
      case dUp    : return [x  ,y-1] ; 
      case dDown  : return [x  ,y+1] ; 
      case dLeft  : return [x-1,y  ] ; 
      case dRight : return [x+1,y  ] ; 
      case dStay  : return [x  ,y  ] ; 
    }
  },


//getSortedPoses :: World -> Pos -> [Pos] -> [Pos]
//getSortedPoses w pos poses = 
//  map snd . sort . map (\pos'->(dist pos pos',pos')) $ poses

  getSortedPoses : function( pos , poses ){


    return _.chain( poses )
            .map( function( p ){ return [ dist(pos,p) , p ]; }  )
            .sortBy( function(x){return x[0];} )
            .map(  function(x){return x[1];} )
            .value();


  },

  getNearestPos : function( pos , poses ){

    var bestPos  = undefined ;
    var bestDist = 999999999 ;

    for( var i in poses ){
      var actPos = poses[i];
      var dista = dist( pos , actPos );
      if( dista < bestDist  ){
        bestDist = dista;
        bestPos  = actPos;
      }
    }

    if( bestPos === undefined ){ return null ;}
    return bestPos;
  },

  isSameAs : function( pos1 , pos2 ){
    if( pos1 === undefined && pos2 !== undefined ){ return false; }
    if( pos1 !== undefined && pos2 === undefined ){ return false; }

    return ( pos1[0] == pos2[0] && pos1[1] == pos2[1] ) ;
  },

  getNearestApplePos : function( pos ){
    return this.getNearestPos( pos , this.applePoses );
  },

  getNearestFlyPos : function( pos ){
    return this.getNearestPos( pos , this.fliesToDo.concat( this.doneFlies ) );
  },







  mapaAt : function( pos ){
    return this.mapa[pos[0]][pos[1]] ;
  },

  drawMapa : function( ){

    var mapa = this.mapa;

    this.ctx.fillStyle = '#CFE6FF' ;
    this.ctx.fillRect(0, 0, 16*40.5 , 16*40.5 );

    for( var y in mapa ){
      for( var x in mapa[y] ){
        var obj = mapa[x][y] ;
        switch( obj.type ){
          case 'wall'  : this.drawWall(x,y);     break;
          case 'apple' : this.drawApple(x,y) ;    break;
          case 'fly'   : this.drawFly(mapa,x,y); break;
        }
      }
    }

    $('#fly-info-container').html(
      'fliesToDo  : ' + this.showPoses( this.fliesToDo  ) + '<br>' + 
      'doneFlies  : ' + this.showPoses( this.doneFlies  ) + '<br>' + 
      'applePoses : ' + this.showPoses( this.applePoses ) + '<br>'  
      
      );

  },

  showPoses : function( poses ){
    
    var ret = '[ ';

    for( var i in poses ){
      ret += '(' + poses[i][0] + ',' + poses[i][1] + '), ' ;
    }

    return ret + ' ]' ;
  },

  drawFly : function( mapa , x_ , y_  ){

    var x = x_*16;
    var y = y_*16;

    this.ctx.drawImage( this.flyImg , x-8 , y );

    if( mapa[x_][y_].progName === '_'  ){
        this.ctx.beginPath();
        this.ctx.arc(x+10, y+10, 22 , 0, 2 * Math.PI, false);
        this.ctx.lineWidth = 1;
        this.ctx.strokeStyle = '#FFFF00';
        this.ctx.stroke();
    }

    this.ctx.fillStyle = "black";
    this.ctx.font = "bold 9px Arial";
    this.ctx.fillText( mapa[x_][y_].energy , x+25, y-10 );

  },

  drawApple : function(x,y){
    this.ctx.drawImage( this.appleImg , x*16 - 2 , y*16 -4);
  },

  drawWall : function(x,y){
    this.ctx.drawImage( this.wallImg , x*16 + 3  , y*16 +3 );
  },



};






});