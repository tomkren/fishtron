





var mkZipper = function(obj,parentZipper){

  var pz = function(prop,defa){
    if( obj[prop] !== undefined ){ return obj[prop];} 
    else if( parentZipper )      { return parentZipper[prop];} 
    else                         { return defa ;}
  };

  return {
    c       : ZIPPER ,
    act     : pz('act'),
    zips    : pz('zips',empty),
    nextVar : pz('nextVar',0),
    numUnfs : pz('numUnfs',0)
  };
};







var mkAppLZ = function(m){
  assert( isApp(m) , 'Error in mkAppLZ!');
  return {
    c : 'appLZ' ,
    n : m.n ,
    t : m.t 
  };
};

var mkAppRZfromLZ = function(lz,m){
  assert( lz.c === 'appLZ' );
  return {
    c : 'appRZ' ,
    m : m ,
    t : lz.t 
  };
};

var mkLamZ = function(m){
  assert( isLam(m) , 'Error in mkLamZ!');
  return {
    c : 'lamZ',
    x : m.x ,
    t : m.t
  };
};




var gotoTop = function( zipper ){
  while( zipper.zips ){
    zipper = goUp(zipper);
  }
  return zipper;
};

var goUp = function( zipper ){
  assert( zipper.zips , 'goUp : impossible to go up ' );
  var headZip = zipper.zips.head ;
  assert( headZip , 'goUp : incorrect headZip..' );

  switch( headZip.c ){
    case 'appLZ' : return mkZipper({
        act  : mkApp( zipper.act , headZip.n ) ,
        zips : zipper.zips.tail
      },zipper);
    case 'appRZ' : return mkZipper({
        act  : mkApp( headZip.m , zipper.act ),
        zips : zipper.zips.tail 
      },zipper);
    case 'lamZ' : return mkZipper({ 
        act  : mkLam_( headZip.x , zipper.act , headZip.t ),
        zips : zipper.zips.tail 
      },zipper);
    default : throw "Unsupported zip constructor."
  }
};

var gotoNextUnf = function( zipper ){
  while( zipper !== null && ! isUnf(zipper.act) ){
    zipper = dfsStep(zipper);
  }
  return zipper;
};

var dfsStep = function( zipper ){
  switch( zipper.act.c ){
    case APP :
      return mkZipper({ 
        act  : zipper.act.m ,
        zips : cons( mkAppLZ(zipper.act) , zipper.zips ) 
      },zipper);
    case LAM :
      return mkZipper({
        act  : zipper.act.m ,
        zips : cons( mkLamZ(zipper.act) , zipper.zips )
      },zipper);
    default : return stepToNextBigger( zipper );
  }
};

var stepToNextBigger = function( zipper ){
  if( !zipper.zips ){ return null; }
  var headZip = zipper.zips.head ;
  if( !headZip ){ return null; }

  switch( headZip.c ){
    case 'appLZ' :
      return mkZipper({
        act  : headZip.n ,
        zips : cons( mkAppRZfromLZ(headZip,zipper.act)  
                   , zipper.zips.tail ) 
      },zipper);
    case 'appRZ' :
      return stepToNextBigger(mkZipper({
        act  : mkApp( headZip.m , zipper.act ),
        zips : zipper.zips.tail 
      },zipper));
    case 'lamZ'  :
      return stepToNextBigger(mkZipper({ 
        act  : mkLam_( headZip.x , zipper.act , headZip.t ),
        zips : zipper.zips.tail 
      },zipper));
    default : throw "Unsupported zip constructor."
  }

};
