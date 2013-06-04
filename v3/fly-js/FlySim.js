
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

  var posPlusDir = function( pos , dir ){
    var x = pos[0];
    var y = pos[1];

    switch( dir ){
      case dUp    : return [x  ,y-1] ; 
      case dDown  : return [x  ,y+1] ; 
      case dLeft  : return [x-1,y  ] ; 
      case dRight : return [x+1,y  ] ; 
    }
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

  var dUp     = 'u' ; //0;
  var dDown   = 'd' ; //1;
  var dLeft   = 'l' ; //2; 
  var dRight  = 'r' ; //3; 

  var mkDefaultRegs = function(){
    return {
      x : 0,
      y : 0,
      z : 0,
      d : dRight
    };
  };

  var progLib = {
      prog1 : function(inp){
        return { move : { type : 'travel' , dir : inp.nAppleDir } ,
                 regs : inp.myRegs };
      },

      prog2 : function(inp){ 
        return { move : { type : 'travel' , dir : dRight } , 
                 regs : inp.myRegs };  
      },

      prog3 : function(inp){
        return { move : { type : 'travel' , dir : inp.nFlyDir } ,
                 regs : inp.myRegs };
      },

      prog4 : function(inp){
        if( inp.myRegs.x > 5 ){

          inp.myRegs.x = 0;

          return {
            move : { type   : 'split' , 
                     dir    : dDown ,  
                     energy : Math.floor(inp.myEnergy/2) ,
                     regs   : mkDefaultRegs() },
            regs : inp.myRegs
          };
        } else {

          if( inp.myRegs.y > 5 ){
            
            inp.myRegs.x ++;

            return { move : { type : 'travel' , dir : inp.nAppleDir } ,
                     regs : inp.myRegs };

          } else {

            inp.myRegs.x ++;
            inp.myRegs.y ++;

            return { move : { type : 'travel' , dir : dRight } ,
                     regs : inp.myRegs };

          }

        }
      }


//    prog i = if (xGet i) > 5 
//             then Output ( Split DDown (myEnergy i `div` 2) defaultRegs ) ( xSet 0 $ myRegs i) 
//             else if (yGet i) > 5
//                  then Output ( Travel (nAppleDir i) ) ( xInc        $ myRegs i)
//                  else Output ( Travel DRight        ) ( xInc . yInc $ myRegs i) 


    };



//  progLib : {
//    prog1 : function (x0){return output_(posToDir_(  myPos_(x0)   , head_(myApplePoses_(x0))  )   );},
//    prog2 : function (x0){return output_(dRight);},
//    prog3 : function (x0){return output_(posToDir_(  myPos_(x0)   , nearestFlyPos_(x0) )  );},
//  },


  // FlySim object
  return {

    progLib : progLib ,

    // returns new sim instance
    init : function( containerID , afterFun ) {

      // (private) members

      var sim            ;

      var playState      ;

      var levelJSON      ;

      var currStep       ;
      var currBigStep    ;
      var currProg       ;
          
      var mapa           ;
      var fliesToDo      ;
      var doneFlies      ;
      var applePoses     ;
      
      var solutionFlyPos ;
      var numSteps       ;
      var numSmallSteps  ;
    
      var container      ;
      var ctx            ;
      var infoDiv        ;

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


        var sliderPlace = function( name ,id , width , ods , bonus ){
          return '<table><tr><td><div>'+name+'</div></td>' +
                 '<td width="'+ods+'px"></td>'+
                 '<td><div id="'+id+'" style="font-size: 75%;width:'+width+'px"></div></td>'+
                 '<td width="10px"></td>'+
                 '<td>'+(bonus || '')+'</td>'+
                 '</tr></table><br>' ;
        };

        var controlsDiv = $('<div>').html(
         sliderPlace( 'Speed'    , 'fly-speed-slider' , 100 , 30 , 
         '<a id="_play-link" href="#">Play</a>' ) +
         sliderPlace( 'Steps'  , '_steps-slider'      , 500 , 40 ) +
         sliderPlace( 'Rounds' , '_big-steps-slider'  , 500 , 27 ) +

         '<select id="_level-select">'+
         '  <option value="w0">Level 0</option>'+
         '  <option value="w1">Level 1</option>'+
         '</select>'

          );

        container.append( controlsDiv ).append( canvas ).append( infoDiv );

        $("#_play-link").click(togglePlay);

        $( "#fly-speed-slider" ).slider({
          range: "min",
          value: 50 ,
          min: 0,
          max: 100,
          slide: function( event, ui ) {
            var x = ui.value;

            if( x <= 50 ){
              timeout = -19*x + 1000 ;
            } else{
              timeout = -x + 100 ;
            }


            //timeout = 0.18*x*x - 28*x + 1000;
            log(timeout);
          }
        });

        $('#_level-select').change(function(){
          pause();
          loadLevel( Levels[ $('#_level-select').val() ] , currProg );
        });


      };

      var restartLevel = function(){
        loadLevel( levelJSON , currProg );
        return sim;
      }

      var loadLevel = function( lvlJSON , prog ){
        
        var level = JSON.parse( lvlJSON );
        levelJSON = lvlJSON ; 

        currStep    = 0 ;
        currBigStep = 0 ;   
        playState   = 'pause' ;      
        
        mapa           = level.mapa ;
        fliesToDo      = level.fliesToDo;
        doneFlies      = level.doneFlies;
        applePoses     = level.applePoses;
                        
        numSteps       = level.numSteps;
        numSmallSteps  = level.numSmallSteps;
        solutionFlyPos = level.solutionFlyPos ;

        mapa[solutionFlyPos[0]][solutionFlyPos[1]] = {  
          type       : 'fly' ,
          progName   : '_'   ,
          energy     : 1     ,
          wasSuccess : true  ,
          lastTravel : dRight,
          regs       : mkDefaultRegs()
        };
    
        fliesToDo.unshift( solutionFlyPos );
    
        for( var y in mapa ){
          for( var x in mapa[y] ){
            var obj = mapa[x][y] ;
            if( obj.type === 'fly' ){

              obj.prog       = progLib[ obj.progName ] ;  // <------------------------
              obj.wasSuccess = true  ;
              obj.lastTravel = dRight;
              obj.regs       = mkDefaultRegs();

            } else if( obj.type === 'apple' ){

              obj.energy = obj.energy || 1 ;

            }

          }
        }



        $( "#_big-steps-slider" ).slider({
          range: "min",
          value: 0 ,
          min: 0,
          max: numSteps,
          slide: function( event, ui ) {
            pause();
            goToBigStep( ui.value );
            $("#_steps-slider").slider('value',currStep);
          }
        });

        $( "#_steps-slider" ).slider({
          range: "min",
          value: 0 ,
          min: 0,
          max: numSmallSteps,
          slide: function( event, ui ) {
            pause();
            goToStep( ui.value );
            $("#_big-steps-slider").slider('value',currBigStep);            
          }
        });

        loadSolution( prog );

        drawMapa();
        return sim;
      }; 

      var loadSolution = function( prog ){
        mapaAt( solutionFlyPos ).prog = prog ;
        currProg = prog ;
        return sim ;
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
          'fliesToDo     : ' + showPoses( fliesToDo  ) + '<br>' + 
          'doneFlies     : ' + showPoses( doneFlies  ) + '<br>' + 
          'applePoses    : ' + showPoses( applePoses ) + '<br>' +
          'numSteps      : ' + numSteps                + '<br>' +
          'numSmallSteps : ' + numSmallSteps           + '<br>' +
          'currStep      : ' + currStep                + '<br>' +
          'currBigStep   : ' + currBigStep             + '<br>' 
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

        var fly = mapa[x_][y_];
    
        ctx.drawImage( flyImg , x-8 , y );
    
        if( fly.progName === '_'  ){
            ctx.beginPath();
            ctx.arc(x+10, y+10, 22 , 0, 2 * Math.PI, false);
            ctx.lineWidth = 1;
            ctx.strokeStyle = '#FFFF00';
            ctx.stroke();
        }
    
        ctx.fillStyle = "black";
        ctx.font = "bold 9px Arial";
        ctx.fillText( fly.energy , x+25, y-10 );

        if( !true ){
          var info     = 'su='+ fly.wasSuccess + ' lt=' + fly.lastTravel
          var regsInfo = 'x=' + fly.regs.x + 
                        ' y=' + fly.regs.y + 
                        ' z=' + fly.regs.z + 
                        ' d=' + fly.regs.d ; 
          
          ctx.fillText( info     , x - 20 , y+39 )
          ctx.fillText( regsInfo , x - 20 , y+49 )
        }
    
      };

      var showPoses = function( poses ){
    
        var ret = '[ ';
    
        for( var i in poses ){
          ret += '(' + poses[i][0] + ',' + poses[i][1] + '), ' ;
        }
    
        return ret + ' ]' ;
      };

      // Simulation :

      var togglePlay = function(){
        if( playState === 'pause' ){
          play();

        } else{
          pause();
        }

        log('PlayState : ' + playState );        
      };

      var play = function(){
        playState = 'play' ;
        $('#_play-link').html('Pause');
        run();
      };

      var pause = function(){
        playState = 'pause' ;
        $('#_play-link').html('Play');
      };

      var timeout = 50 ;

      var run = function(n){
        if(n<=0){return;}
        ( timeout > 500 ? steps : bigSteps )(1);
        setTimeout( function(){
          if( playState == 'pause' ) return;
          $('#_steps-slider'    ).slider('value',currStep);
          $('#_big-steps-slider').slider('value',currBigStep); 
          run(n-1); 
        } , timeout ); 
      };


      var goTo = function( n , stepsFun , currVal ){
        if( n < currVal ){ restartLevel(); return stepsFun( n           );  } 
        else             {                 return stepsFun( n - currVal );  }        
      };

      var goToBigStep= function( n ){
        return goTo( n , bigSteps , currBigStep );

     
      };

      var goToStep = function( n ){
        return goTo( n , steps , currStep );    
      };

      var bigSteps = function(numSteps){
        numSteps = (numSteps!== 0) ? (numSteps || 1) : 0 ;

        for( var i = 0 ; i < numSteps ; i++ ){
          while( fliesToDo.length > 0 ){
            step();
          }  
          prepareNewRound();
        }
        
        drawMapa();
        return sim;
      };

      var prepareNewRound = function(){
        fliesToDo = doneFlies.reverse();
        doneFlies = [];

        currBigStep ++ ;
      };

      var steps = function( numSteps ){
        numSteps = (numSteps!== 0) ? (numSteps || 1) : 0 ;
        for( var i = 0 ; i < numSteps ; i++ ){
          step();
        }

        drawMapa();
        return sim;
      };

      var step = function(){
    
        if( fliesToDo.length == 0 ){ 
          prepareNewRound();
          //throw "There should be a fly!" ; 
        }
    
        var flyPos = fliesToDo.shift();
        var fly    = mapaAt( flyPos );

        var input      = prepareInput( flyPos , fly );
        var output     = fly.prog( input ); 
        var moveResult = reactToOutput( output , flyPos );
        
        //log( input );
        //log( output );
        //log( moveResult );

        currStep ++ ;
        return sim;
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

      };

      var nearestInfo = function( pos , poses ){
        if( poses.length == 0 ){
          return { dist: 999999, dir : dRight , energy : 0 } ;
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
          return { dist: 999999, dir : dRight , energy : 0 } ;
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

      var reactToOutput = function( output , flyPos ){

        var moveResult = performMove( flyPos , output.move );

        if( hasCurrFlySurvived( moveResult ) ){
              var fly = mapaAt( moveResult.newPos );

              fly.wasSuccess = moveResult.success ;
              fly.regs = output.regs;
              if( output.move.type == 'travel' ){ fly.lastTravel = output.move.dir; }
        }

        stepFliesQueue( moveResult );

        return moveResult;

      };

      var hasCurrFlySurvived = function( moveResult ){
        return moveResult.moveRes != 'currDead' ;
      };

      var stepFliesQueue = function( moveResult ){

        switch( moveResult.moveRes ){
          case 'std' : 
            doneFlies.unshift( moveResult.newPos );
            break;
          case 'currDead'  : break;
          case 'otherDead' : 
            deleteFromPoses( fliesToDo , moveResult.newPos );
            deleteFromPoses( doneFlies , moveResult.newPos );
            doneFlies.unshift( moveResult.newPos );   
            break;
          case 'split' : 
            fliesToDo.unshift( moveResult.childPos );
            doneFlies.unshift( moveResult.newPos   )
            break;
        }
      };



      var performMove = function( flyPos , move ){
        switch( move.type ){
          case 'travel' : return doTravel( flyPos , move.dir                          ); 
          case 'split'  : return doSplit ( flyPos , move.dir, move.energy , move.regs ); 
          default       : throw 'Unsupported move type!' ;
        }
      };

      var doTravel = function( flyPos , dir ){

        var travelPos = posPlusDir( flyPos , dir );
        var obj       = mapaAt( travelPos ); 

        switch( obj.type ){
          case 'free'  : moveFromTo( flyPos , travelPos ) ; 
                         return { moveRes : 'std' , newPos : travelPos , success : true }; 
          case 'apple' : eatApple( obj.energy , flyPos , travelPos ) ; 
                         return { moveRes : 'std' , newPos : travelPos , success : true }; 
          case 'fly'   : return flyCollision(   flyPos , travelPos ) ;
          case 'wall'  : return { moveRes : 'std' , newPos : flyPos    , success : false };
          default      : throw 'Unsupported mapa-obj type!'
        }

      };

      var doSplit = function( flyPos , dir , childEnergy , childRegs ){

        var childPos = posPlusDir( flyPos , dir );
        var obj      = mapaAt( childPos );

        if( obj.type != 'free' ){
          return { moveRes : 'std' , newPos : flyPos , success : false }; 
        } 

        var motherFly = mapaAt(flyPos);
        var correctChildEnergy = Math.min( childEnergy , motherFly.energy - 1 );

        if( correctChildEnergy <= 0 ){
          return { moveRes : 'std' , newPos : flyPos , success : false };  
        }

        var childFly = mkChild( motherFly , correctChildEnergy , childRegs, dir );
        putObjOnPos( childPos , childFly ); 
        motherFly.energy -= correctChildEnergy; 

        return { moveRes : 'split' , newPos : flyPos , childPos : childPos , success : true };       
      };


      var mkChild = function( motherFly , childEnergy , childRegs, bornDir ){
        return {
          type       : 'fly' ,
          progName   : motherFly.progName  ,
          prog       : motherFly.prog      ,
          energy     : childEnergy         ,
          wasSuccess : true                ,
          lastTravel : bornDir             ,
          regs       : childRegs
        };
      };



      var moveFromTo = function( from , to ){
        var obj = mapaAt( from );
        deleteOnPos( from );
        putObjOnPos( to , obj );
        return obj;
      };

      var eatApple = function( appleEnergy , flyPos , applePos ){
        var fly = moveFromTo( flyPos , applePos );
        fly.energy += appleEnergy ;
        deleteFromPoses( applePoses , applePos );
      };

      var flyCollision = function( herPos , oponentPos ){

        var herEnergy = mapaAt(    herPos).energy ; 
        var opoEnergy = mapaAt(oponentPos).energy ;

        if( herEnergy >= opoEnergy ){
          var she = moveFromTo( herPos , oponentPos );
          she.energy += opoEnergy ;

          return { moveRes : 'otherDead' , newPos : oponentPos , success : true };

        } else {
          deleteOnPos(herPos);
          mapaAt(oponentPos).energy += herEnergy ;

          return { moveRes : 'currDead' , success : false };
        }
      };



      var putObjOnPos = function( pos , obj ){
        mapa[pos[0]][pos[1]] = obj;
      };

      var deleteOnPos = function( pos ){
        putObjOnPos( pos , { type : 'free' } );
      };

      var deleteFromPoses = function( poses , what ){
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
      };

    

      var mapaAt = function( pos ){
        var row = mapa[pos[0]];
        return (row && row[pos[1]])  ||  { type : 'free' } ;
      };

      // INIT CODE :

      sim =
        { loadLevel    : loadLevel 
        , restartLevel : restartLevel    
        , loadSolution : loadSolution  
        , steps        : steps         
        , bigSteps     : bigSteps      
        , goToBigStep  : goToBigStep 
        , goToStep     : goToStep   
        , run          : run     

        , mapaAt : mapaAt };

      var imgs = loadImages( 
        ["img/wall.png", "img/fly.png", "img/apple.png" ], 
        function(){
          initHTML();
          afterFun( sim );
        });

      wallImg  = imgs[0];
      flyImg   = imgs[1];
      appleImg = imgs[2];

      //new sim instance 
      return sim;
    }

  };


}();






 


