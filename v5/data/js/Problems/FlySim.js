
var FlySim = function(){

  var log = function(x){ console.log(x) };

  var cur2 = function( fun ){
    return function( x , y ){
      if( y === undefined ){
        return function(y){ return fun(x,y) ; }
      } else {
        return fun( x , y );
      }
    }
  }; 
  
  var cur3 = function( fun ){
    return function( x , y , z ){
      if( y === undefined ){
        return cur2( function(y,z){return fun(x,y,z);} );
      } else if( z === undefined ){
        return function(z){return fun(x,y,z);} ;
      } else {
        return fun(x,y,z);
      }
    }
  };
  

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


  var output_ = cur2(function( move , regs ){
    return { move : move , regs : regs };
  });

  var if_ = cur3(function(p,q,r){
    if (p) {return q;} else {return r;}
  });

  var travel_ = function( dir ){
    return { type : 'travel' , dir : dir };
  };

  var split_ = cur3(function( dir , energy , regs ){
    return { type   : 'split' , 
             dir    : dir ,  
             energy : energy ,
             regs   : regs }
  });

  var myEnergy_     = function(input){ return input.myEnergy     ; };         
  var myLastTravel_ = function(input){ return input.myLastTravel ; };
  var myWasSuccess_ = function(input){ return input.myWasSuccess ; };
  var nAppleDir_    = function(input){ return input.nAppleDir    ; };
  var nAppleDist_   = function(input){ return input.nAppleDist   ; };
  var nAppleEnergy_ = function(input){ return input.nAppleEnergy ; };
  var nFlyDir_      = function(input){ return input.nFlyDir      ; };
  var nFlyDist_     = function(input){ return input.nFlyDist     ; };
  var nFlyEnergy_   = function(input){ return input.nFlyEnergy   ; };
  var cAppleDir_    = function(input){ return input.cAppleDir    ; };
  var cAppleDist_   = function(input){ return input.cAppleDist   ; };
  var myRegs_       = function(input){ return input.myRegs       ; };

  var xGet_ = function(input){ return input.myRegs.x }; 
  var yGet_ = function(input){ return input.myRegs.y }; 
  var zGet_ = function(input){ return input.myRegs.z }; 
  var dGet_ = function(input){ return input.myRegs.d }; 

  var xSet_ = cur2(function(i,regs){ return regsSet('x',i,regs); });
  var ySet_ = cur2(function(i,regs){ return regsSet('y',i,regs); });
  var zSet_ = cur2(function(i,regs){ return regsSet('z',i,regs); });
  var dSet_ = cur2(function(i,regs){ return regsSet('d',i,regs); });

  var xInc_ = function(regs){ return regsInc('x',regs); };
  var yInc_ = function(regs){ return regsInc('y',regs); };
  var zInc_ = function(regs){ return regsInc('z',regs); };
  
  var mkDefaultRegs = function(){
    return {
      x : 0,
      y : 0,
      z : 0,
      d : dRight
    };
  };

  var defaultRegs_ = mkDefaultRegs() ;

  var copyRegs = function( rs ){
    return {x:rs.x,y:rs.y,z:rs.z,d:rs.d};
  };

  var regsSet = function( varName , newVal , regs ){
    var ret  = copyRegs(regs); 
    ret[varName] = newVal; 
    return ret;
  };

  var regsInc = function( varName , regs ){
    var ret  = copyRegs(regs); 
    ret[varName] ++ ; 
    return ret;
  };

  var rotCW_ = function( dir ){
    switch( dir ){
      case dUp    : return dRight ;      
      case dDown  : return dLeft  ;     
      case dLeft  : return dUp    ;      
      case dRight : return dDown  ;       
    }
  };      

  var equals = cur2(function(x,y){
    return x == y ;
  });

  var lte = cur2(function(x,y){ return (x<=y) ; });

  var easySplit = function(inp){
      return split_( dDown , div( myEnergy_(inp) , 2) , myRegs_(inp) );
  };

  var div = cur2(function(x,y){  if(y==0){return 1;} return Math.floor(x/y);  });

  var Funs = {
    
    output_ : output_ ,

    if_     : if_     ,

    travel_ : travel_ ,
    split_  : split_  ,

    myEnergy_     : myEnergy_      ,
    myLastTravel_ : myLastTravel_  ,
    myWasSuccess_ : myWasSuccess_  ,
    nAppleDir_    : nAppleDir_     ,
    nAppleDist_   : nAppleDist_    ,
    nAppleEnergy_ : nAppleEnergy_  ,
    nFlyDir_      : nFlyDir_       ,
    nFlyDist_     : nFlyDist_      ,
    nFlyEnergy_   : nFlyEnergy_    ,
    cAppleDir_    : cAppleDir_     ,
    cAppleDist_   : cAppleDist_    ,
    myRegs_       : myRegs_        ,

    xGet_ : xGet_ ,
    yGet_ : yGet_ ,
    zGet_ : zGet_ ,
    dGet_ : dGet_ ,
    xSet_ : xSet_ ,
    ySet_ : ySet_ ,
    zSet_ : zSet_ ,
    dSet_ : dSet_ ,
    xInc_ : xInc_ ,
    yInc_ : yInc_ ,
    zInc_ : zInc_ ,

    defaultRegs_ : defaultRegs_ ,

    rotCW_ : rotCW_ ,

    dUp     : dUp     ,
    dDown   : dDown   ,
    dLeft   : dLeft   ,
    dRight  : dRight  ,

    div     : div,

    lte : lte,

    easySplit : easySplit

  };


  var progLib_BO2B = {
    //BEST OF #2B


    'B - gen 28 ff 658' : function (x0){return output_(if_(equals(1,zGet_(x0)),if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(nAppleDir_(x0),2,xInc_(ySet_(2,zInc_(myRegs_(x0)))))),split_(nAppleDir_(x0),2,xInc_(ySet_(yGet_(x0),myRegs_(x0))))),zSet_(xGet_(x0),myRegs_(x0)));},
    'B - gen 14 ff 626' : function (x0){return output_(if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(nAppleDir_(x0),2,xInc_(ySet_(2,dSet_(dLeft,zInc_(myRegs_(x0))))))),ySet_(1,dSet_(rotCW_(if_(lte(2,2),dUp,rotCW_(dUp))),zSet_(xGet_(x0),myRegs_(x0)))));},
    'B - gen 7  ff 446' : function (x0){return output_(if_(equals(1,zGet_(x0)),travel_(nAppleDir_(x0)),split_(rotCW_(dUp),2,xInc_(ySet_(2,dSet_(rotCW_(dRight),zInc_(myRegs_(x0))))))),ySet_(1,dSet_(rotCW_(dLeft),myRegs_(x0))));},
    'B - gen 0  ff 339' : function (x0){return output_(travel_(cAppleDir_(x0)),myRegs_(x0));}
  

  };

  var progLib_BO2 = {
    //BEST OF #2

    'A - gen 20 ff 1336' : function (x0){return if_(myWasSuccess_(x0),output_(easySplit(x0),zInc_(myRegs_(x0))),output_(travel_(if_(lte(yGet_(x0),1),dLeft,if_(lte(yGet_(x0),1),dLeft,nAppleDir_(x0)))),xSet_(2,xInc_(xInc_(yInc_(zInc_(xSet_(yGet_(x0),xInc_(dSet_(dLeft,myRegs_(x0)))))))))));},
    'A - gen 17 ff 817'  : function (x0){return if_(myWasSuccess_(x0),output_(split_(dUp,myEnergy_(x0),myRegs_(x0)),myRegs_(x0)),output_(travel_(dGet_(x0)),dSet_(dLeft,dSet_(dGet_(x0),zSet_(2,xInc_(myRegs_(x0)))))));},
    'A - gen 7  ff 815'  : function (x0){return if_(myWasSuccess_(x0),output_(split_(dUp,myEnergy_(x0),myRegs_(x0)),myRegs_(x0)),output_(travel_(if_(lte(yGet_(x0),1),dLeft,rotCW_(dDown))),xSet_(2,xInc_(myRegs_(x0)))));},
    'A - gen 1  ff 489'  : function (x0){return if_(myWasSuccess_(x0),output_(easySplit(x0),ySet_(nAppleEnergy_(x0),yInc_(dSet_(dLeft,myRegs_(x0))))),output_(travel_(dLeft),myRegs_(x0)));},
    'A - gen 0  ff 352'  : function (x0){return if_(myWasSuccess_(x0),output_(travel_(rotCW_(dDown)),zSet_(if_(lte(2,myEnergy_(x0)),1,1),yInc_(yInc_(yInc_(myRegs_(x0)))))),output_(travel_(nAppleDir_(x0)),dSet_(dDown,xSet_(1,myRegs_(x0)))));}

  };


  var progLib_BO1 = {
    //BEST OF #1
  
    'gen 22 ff 199' : function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),nFlyEnergy_(x0)),output_(  travel_(dLeft),myRegs_(x0)),if_(lte(yGet_(x0),1),output_(travel_(dDown),yInc_(myRegs_(x0))),output_(  travel_(nAppleDir_(x0)),myRegs_(x0)))),output_(travel_(dDown),yInc_(dSet_(dDown,myRegs_(x0)))));},
    'gen 13 ff 198' : function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),  myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(dDown),yInc_(zInc_(ySet_(1,  myRegs_(x0))))));},
    'gen 11 ff 197' : function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),  myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(dDown),yInc_(dSet_(dDown,  myRegs_(x0)))));},
    'gen 7 ff 124'  : function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),  myRegs_(x0)),output_(travel_(nAppleDir_(x0)),myRegs_(x0))),output_(travel_(nAppleDir_(x0)),yInc_(dSet_(  dDown,myRegs_(x0)))));},
    'gen 6 ff 109'  : function (x0){return if_(lte(yGet_(x0),nAppleEnergy_(x0)),output_(travel_(dDown),yInc_(  myRegs_(x0))),output_(travel_(nAppleDir_(x0)),yInc_(myRegs_(x0))));},
    'gen 5 ff 68'   : function (x0){return if_(myWasSuccess_(x0),if_(lte(yGet_(x0),1),output_(travel_(dLeft),  myRegs_(x0)),output_(travel_(rotCW_(dRight)),ySet_(2,myRegs_(x0)))),output_(travel_(nAppleDir_(x0)),  yInc_(dSet_(dDown,myRegs_(x0)))));},
    'gen 3 ff 55'   : function (x0){return if_(myWasSuccess_(x0),output_(travel_(dLeft),myRegs_(x0)),output_(  travel_(nAppleDir_(x0)),myRegs_(x0)));},
    'gen 2 ff 49'   : function (x0){return if_(equals(yGet_(x0),1),output_(travel_(rotCW_(dDown)),myRegs_(x0))  ,output_(travel_(if_(myWasSuccess_(x0),dLeft,rotCW_(dLeft))),myRegs_(x0)));},
    'gen 0 ff 30'   : function (x0){return if_(lte(2,nFlyEnergy_(x0)),output_(travel_(dRight),myRegs_(x0)),  output_(travel_(dLeft),dSet_(dLeft,zInc_(myRegs_(x0)))));}
      
  };


  var progLib = {
      prog1 : function(inp){
        return { move : { type : 'travel' , dir : inp.nAppleDir } ,
                 regs : inp.myRegs };
      },

      prog1_ : function(inp){
        return output_( travel_( nAppleDir_( inp ) ) , myRegs_( inp ) );
      },

      prog2 : function(inp){ 
        return { move : { type : 'travel' , dir : dRight } , 
                 regs : inp.myRegs };  
      },

      prog3 : function(inp){
        return { move : { type : 'travel' , dir : inp.nFlyDir } ,
                 regs : inp.myRegs };
      },


// prog4_ i = if' ((xGet_ i) > 5) 
//                (output_ ( split_ dDown (myEnergy_ i `div` 2) defaultRegs_ ) ( xSet_ 0 $ myRegs_ i) )
//                ( if' ((yGet_ i) > 5)
//                      (output_ ( travel_ (nAppleDir_ i) ) ( xInc_         $ myRegs_ i) )
//                      (output_ ( travel_ dRight         ) ( xInc_ . yInc_ $ myRegs_ i) ) )
// 


      prog4_ : function( i ){
        return if_( ( xGet_(i) > 5 ) , 
                    ( output_( split_( dDown , div( myEnergy_(i) , 2) , defaultRegs_ ) , xSet_(0,myRegs_(i) )  ) ) , 
                    ( if_( ( yGet_(i) > 5 ) , 
                           ( output_( travel_( nAppleDir_(i)  ) ,        xInc_(myRegs_(i))      )  ) , 
                           ( output_( travel_( dRight         ) ,  yInc_(xInc_(myRegs_(i)))     )  )      ) ) );
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

      var initEnergy     ;

      var levelsDB       ;
      var solutionsDB    ;

      var playState      ;

      var levelJSON      ;

      var currStep       ;
      var currBigStep    ;
      var currProg       ;
      var currProgName   ;
          
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

      var maxForSteps    ;
      var maxForBigSteps ;

      var wallImg, appleImg, flyImg ;

      // (private) member functions

      var initStructures = function(){
        solutionsDB = {} ;

        //for( var name in progLib ){
        //  solutionsDB[name] = progLib[name] ;
        //}

        initSlidersMaxes();
        initEnergy = 100;

      };

      var initSlidersMaxes = function(){
        maxForSteps    = 64 ;
        maxForBigSteps = 64 ;
      };

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
         sliderPlace( 'Rounds' , '_big-steps-slider'  , 500 , 27 ) ) 
        .append( $('<select>').attr('id','_level-select') )
        .append( $('<select>').attr('id','_solution-select') );

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

          initSlidersMaxes();

          var lvlID = $('#_level-select').val() ;

          pause();
          loadLevel_( JSON.stringify( levelsDB[ lvlID ] )  );
          setSolution( currProgName );
        });

        $('#_solution-select').change(function(){
          
          initSlidersMaxes();

          currProgName = $('#_solution-select').val() ;
          currProg     = solutionsDB[currProgName] ;

          var lvlID = $('#_level-select').val() ;

          pause();
          loadLevel_( JSON.stringify( levelsDB[ lvlID ] )  );
          setSolution( currProgName );
        });

      };

      var loadLevelsDB = function( lvlsDB ){
        levelsDB = lvlsDB;

        var select = $('#_level-select');

        select.html('');

        for( var i in levelsDB ){
          select.append(  $('<option>').attr('value',i).html( i )  );
        }

      };

      var addNewSolution = function( name , prog ){
        loadSolution( name, prog );

        currProgName = $('#_solution-select').val() ;
        currProg     = solutionsDB[currProgName] ;

        var lvlID = $('#_level-select').val() ;
        
        pause();
        loadLevel_( JSON.stringify( levelsDB[ lvlID ] )  );
        setSolution( currProgName );

      };

      var loadSolution = function( name , prog ){
        solutionsDB[name] = prog ;
        drawSolutionSelect();
      };

      var setSolution = function( name ){
        var prog = solutionsDB[name];
        mapaAt( solutionFlyPos ).prog = prog ;
        currProg = prog ;     
        currProgName = name;
      };

      var drawSolutionSelect = function(){
        var select = $('#_solution-select');
        select.html('');

        for( var name in solutionsDB ){
          select.prepend(  $('<option>').attr('value',name).html( name )  );
        }
      };

      var restartLevel = function(){
        loadLevel_( levelJSON );
        setSolution( currProgName );
        return sim;
      }

      var loadLevel = function( lvlID , progName , prog ){
        loadSolution( progName , prog );
        loadLevel_( JSON.stringify( levelsDB[ lvlID ] ) );
        setSolution( progName );
      };



      var loadLevel_ = function( lvlJSON ){
        
        levelJSON = lvlJSON ; 
       
        var level = JSON.parse( levelJSON ) ;

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
          energy     : initEnergy , // 1 nebo  100   ,
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
          max: maxForBigSteps,
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
          max: maxForSteps,
          slide: function( event, ui ) {
            pause();
            goToStep( ui.value );
            $("#_big-steps-slider").slider('value',currBigStep);            
          }
        });

        drawMapa();
        return sim;
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
        if( currBigStep == numSteps || currBigStep == numSteps + 1 ){ return;}

        ( timeout > 500 ? steps : bigSteps )(1);
        setTimeout( function(){
          if( playState == 'pause' ) return;
          $('#_steps-slider'    ).slider('value',currStep);
          $('#_big-steps-slider').slider('value',currBigStep);

          maxForSteps = $('#_steps-slider').slider('option','max');
          if( currStep > maxForSteps ){
            maxForSteps *= 2;
            $('#_steps-slider').slider('option','max', maxForSteps );
          }

          maxForBigSteps = $('#_big-steps-slider').slider('option','max');
          if( currBigStep > maxForBigSteps ){
            maxForBigSteps *= 2;
            $('#_big-steps-slider').slider('option','max', maxForBigSteps );
          }

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
        , addNewSolution: addNewSolution
        , play         : play
        , steps        : steps         
        , bigSteps     : bigSteps      
        , goToBigStep  : goToBigStep 
        , goToStep     : goToStep   
        , run          : run 
        , loadLevelsDB : loadLevelsDB 
        , Funs         : Funs  

        , setInitEnergy : function(x){ initEnergy = x ; log('init energy:'+initEnergy);} 


        , mapaAt : mapaAt
        , getSolutionsDB : function(){return solutionsDB;} 
        , getLevelsDB    : function(){return levelsDB;}    
        
        , get_BO1  : function(){return progLib_BO1;}
        , get_BO2  : function(){return progLib_BO2;}
        , get_BO2B : function(){return progLib_BO2B;}


        };

      var imgs = loadImages( 
        ["img/wall.png", "img/fly.png", "img/apple.png" ], 
        function(){
          initStructures();
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






 


