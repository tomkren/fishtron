
var FlySim = function(){

  var log = function(x){ console.log(x) };

  var minus = function( pos1 , pos2 ){ 
    return [ pos1[0]-pos2[0] , pos1[1]-pos2[1] ]; 
  };
  
  var dist = function( pos1 , pos2 ) {
      var d = minus( pos1 , pos2 );
      var dx = d[0] , dy = d[1];
      return Math.sqrt(  dx*dx + dy*dy );
  };

  var getNearestPos = function( pos , poses ){
 
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
  
    //log(pos +' - ' + bestPos+' - '+bestDist);

    if( bestPos === undefined ){ return null ;}
    return [bestPos,bestDist] ;
  };

  var posToDir = function( posMy , posHer ){ 
         
    var d = minus( posHer , posMy );
    var dx = d[0] , dy = d[1];
         
    if(   dx  > dy && (-dx) > dy ) { return dUp   ; }
    if(   dx  > dy               ) { return dRight; }
    if( (-dx) > dy               ) { return dLeft ; }
    if( true                     ) { return dDown ; }
        
  };


  var loadImages = function( myImages , afterFun ) {
  
    var imgs = [];
  
    var imageCount = myImages.length;
    var loadedCount = 0, errorCount = 0;
  
    var checkAllLoaded = function() {
      if (loadedCount + errorCount == imageCount ) {
         afterFun();
      }
    };
  
    var onload = function() {
      loadedCount++;
      checkAllLoaded();
    }, onerror = function() {
      errorCount++;
      checkAllLoaded();
    };   
  
    for (var i = 0; i < imageCount; i++) {
      var img = new Image();
      img.onload = onload; 
      img.onerror = onerror;
      img.src = myImages[i];
  
      imgs[i] = img;
    }
  
    return imgs;
  };

  var dUp     = 0;
  var dDown   = 1;
  var dLeft   = 2; 
  var dRight  = 3; 

  var mkDefaultRegs = function(){
    return {
      x : 0,
      y : 0,
      z : 0,
      d : dRight
    };
  };

  // FlySim object
  return {

    // returns new sim instance
    init : function( containerID , afterFun ) {

      // (private) members

      var levelJSON  ;
      
      var mapa       ;
      var fliesToDo  ;
      var doneFlies  ;
      var applePoses ;
      
      var numSteps   ;

      var container  ;
      var ctx        ;
      var infoDiv    ;

      var wallImg, appleImg, flyImg ;

      // (private) member functions

      var initHTML = function(){


        container = $( '#' + containerID );

        var canvas = $('<canvas>') 
                     .attr( 'width'  , 666 )
                     .attr( 'height' , 666 );

        ctx = canvas[0].getContext('2d');

        infoDiv = $('<div>')
                  .css('font-size','66%');

        container.append( canvas ).append( infoDiv );
      };

      var loadLevel = function( lvlJSON ){
        
        var level = JSON.parse( lvlJSON );
        levelJSON = lvlJSON ;          
        mapa = level.mapa ;
        mapa[level.solutionFlyPos[0]][level.solutionFlyPos[1]] = {  
          type       : 'fly' ,
          progName   : '_'   ,
          energy     : 1     ,
          wasSuccess : true  ,
          lastTravel : dRight,
          regs       : mkDefaultRegs()
        };
        fliesToDo  = level.fliesToDo;
        doneFlies  = level.doneFlies;
        applePoses = level.applePoses;
                    
        numSteps   = level.numSteps;
    
        fliesToDo.unshift( level.solutionFlyPos );
    
        for( var y in mapa ){
          for( var x in mapa[y] ){
            var obj = mapa[x][y] ;
            if( obj.type === 'fly' ){
              //obj.prog = progLib[ obj.progName ] ;  <----------------- TODO
              obj.wasSuccess = true  ;
              obj.lastTravel = dRight;
              obj.regs       = mkDefaultRegs();
            }
          }
        }
        drawMapa();
      }; 

      // Drawing :

      var drawMapa = function(){
        
        ctx.fillStyle = '#CFE6FF' ;
        ctx.fillRect(0, 0, 16*40.5 , 16*40.5 );

        for( var y in mapa ){
          for( var x in mapa[y] ){
            var obj = mapa[x][y] ;
            switch( obj.type ){
              case 'wall'  : drawWall(x,y);     break;
              case 'apple' : drawApple(x,y) ;   break;
              case 'fly'   : drawFly(mapa,x,y); break;
            }
          }
        }

        infoDiv.html(
          'fliesToDo  : ' + showPoses( fliesToDo  ) + '<br>' + 
          'doneFlies  : ' + showPoses( doneFlies  ) + '<br>' + 
          'applePoses : ' + showPoses( applePoses ) + '<br>'  
        );

      };

      var drawWall = function(x,y){
        ctx.drawImage( wallImg , x*16 + 3  , y*16 +3 );
      };

      var drawApple = function(x,y){
        ctx.drawImage( appleImg , x*16 - 2 , y*16 -4);
      };

      var drawFly = function( mapa , x_ , y_  ){
    
        var x = x_*16;
        var y = y_*16;
    
        ctx.drawImage( flyImg , x-8 , y );
    
        if( mapa[x_][y_].progName === '_'  ){
            ctx.beginPath();
            ctx.arc(x+10, y+10, 22 , 0, 2 * Math.PI, false);
            ctx.lineWidth = 1;
            ctx.strokeStyle = '#FFFF00';
            ctx.stroke();
        }
    
        ctx.fillStyle = "black";
        ctx.font = "bold 9px Arial";
        ctx.fillText( mapa[x_][y_].energy , x+25, y-10 );
    
      };

      var showPoses = function( poses ){
    
        var ret = '[ ';
    
        for( var i in poses ){
          ret += '(' + poses[i][0] + ',' + poses[i][1] + '), ' ;
        }
    
        return ret + ' ]' ;
      };

      // Simulation :

      var stepCurrentFly = function(){
    
        if( fliesToDo.length == 0 ){ throw "There should be a fly!" ; }
    
        var currentFlyPos = fliesToDo.shift();
        var fly           = mapaAt( currentFlyPos );
        var input         = prepareInput( currentFlyPos , fly );

        log( input );

        drawMapa();

        //var output        = fly.prog( input ); 
        
        //var newFlyPos     = this.tryToMoveFly( output , currentFlyPos ) ;
        
        //this.doneFlies.unshift( newFlyPos );
    
      };

      var prepareInput = function( flyPos , fly ){

        var nAppleInfo = nearestInfo( flyPos , applePoses );
        //předpoklad: ve fliesToDo ++ doneFlies neni ta flyPos
        var nFlyInfo   = nearestInfo( flyPos , fliesToDo.concat(doneFlies) );
        
        var cAppleInfo =  centerInfo( flyPos , applePoses );

        return {
          myEnergy     : fly.energy        ,
          myLastTravel : fly.lastTravel    ,
          myWasSuccess : fly.wasSuccess    ,

          nAppleDir    : nAppleInfo.dir    ,  
          nAppleDist   : nAppleInfo.dist   ,
          nAppleEnergy : nAppleInfo.energy ,

          nFlyDir      : nFlyInfo.dir      ,
          nFlyDist     : nFlyInfo.dist     ,
          nFlyEnergy   : nFlyInfo.energy   ,

          cAppleDir    : cAppleInfo.dir    , 
          cAppleDist   : cAppleInfo.dist   ,

          myRegs       : fly.regs
        };

        // return [ this.getSortedPoses( flyPos , this.applePoses ) ,
        //          this.getNearestFlyPos( flyPos ) ,
        //          flyPos,
        //          fly.energy ];
      };

      var nearestInfo = function( pos , poses ){
        if( poses.length == 0 ){
          return [ 999999, dRight , 0 ] ;
        }

        var res = getNearestPos(pos,poses);
        var nPos = res[0], nDist = res[1];

        return { dir    : posToDir( pos , nPos ) , 
                 dist   : nDist , 
                 energy : mapaAt(nPos).energy || 1 } ;
      };

      // returns [ Dist , Dir ] of weighted center
      var centerInfo = function( pos , poses ){
        if( poses.length == 0 ){
          return [ 999999, dRight , 0 ] ;
        }

        var weightedXSum = 0;
        var weightedYSum = 0;
        var energySum = 0;

        for( var i = 0 ; i < poses.length ; i++ ){
          
          var xPos = poses[i][0];
          var yPos = poses[i][1];
          var en   = mapaAt(poses[i]).energy || 1;

          weightedXSum += xPos * en;
          weightedYSum += yPos * en; 
          energySum    += en;

        }   

        var centerPos = [ weightedXSum / energySum , 
                          weightedYSum / energySum ];

        return {
          dist :     dist( pos , centerPos ),
          dir  : posToDir( pos , centerPos )
        };

      };

      var mapaAt = function( pos ){
        return mapa[pos[0]][pos[1]] ;
      };

      // INIT CODE :

      var imgs = loadImages( 
        ["img/wall.png", "img/fly.png", "img/apple.png" ], 
        function(){
          initHTML();
          afterFun();
        });

      wallImg  = imgs[0];
      flyImg   = imgs[1];
      appleImg = imgs[2];

      //new sim instance 
      return {
        loadLevel      : loadLevel ,
        stepCurrentFly : stepCurrentFly
      };
    }

  };


}();






 


