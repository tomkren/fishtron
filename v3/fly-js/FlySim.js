
var FlySim = function(){

  var log = function(x){ console.log(x) };



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
          energy     : 1  
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
            }
          }
        }
        drawMapa();
      }; 

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





      wallImg  = new Image();
      flyImg   = new Image();
      appleImg = new Image();
      
      wallImg .src = 'img/wall.png' ;
      flyImg  .src = 'img/fly.png' ;
      appleImg.src = 'img/apple.png'; 

      wallImg.onload = function(){
        flyImg.onload = function(){
          appleImg.onload = function(){
            initHTML();
            afterFun();
          };
        };
      };
      
      //new sim instance 
      return {
        loadLevel : loadLevel 
      };
    }

  };


}();






 


