
var ATM    = 1;
var ARR    = 2;

var VAR    = 3;
var VAL    = 4;
var APP    = 5;
var LAM    = 6;
var UNF    = 7;

var ZIPPER = 8;



var isArr    = mkTypeChecker(ARR);
var isAtm    = mkTypeChecker(ATM);
var isVar    = mkTypeChecker(VAR);
var isVal    = mkTypeChecker(VAL);
var isApp    = mkTypeChecker(APP);
var isLam    = mkTypeChecker(LAM);
var isUnf    = mkTypeChecker(UNF);
var isTyp    = mkTypeChecker([isArr,isAtm]);
var isTerm   = mkTypeChecker([isVar,isVal,isApp,isLam,isUnf]);
var isZipper = mkTypeChecker(ZIPPER);


var mkAtm = function(a){
  return {
    c : ATM ,
    a : a
  };
};

var mkArr = function(a,b){
  return {
    c : ARR,
    a : a,
    b : b
  };
};

var mkVar = function(x,t){
  return { 
    c : VAR,
    x : x,
    t : t
  };
};

var mkVal = function(x,t){
  return { 
    c : VAL,
    x : x,
    t : t
  };
};

var mkApp = function(m,n){

  assert( isArr( m.t ) );

  return { 
    c : APP,
    m : m,
    n : n,
    t : m.t.b
  };
};

var mkLam_ = function(x,m,t){
  assert( _.isEqual( m.t , t.b )  );
  return { 
    c : LAM,
    x : x,
    m : m,
    t : t
  };
};

var mkLam = function(x,y,z){
  return ( z ? mkLam_fromStr(x,y,z) 
             : mkLam_fromVar(x,y)   ) ;
};

var mkLam_fromStr = function(x,tx,m){
  return {
    c : LAM, 
    x : x,
    m : m,
    t : mkArr( tx , m.t )
  };
};

var mkLam_fromVar = function(x,m){
  return {
    c : LAM, 
    x : x.x,
    m : m,
    t : mkArr( x.t , m.t )
  };
};

var mkUnf = function(t,atmTab){
  return { 
    c      : UNF,
    t      : t,
    atmTab : atmTab 
  };
};




var mkSexpr = function(f,ms){

  var parts = tParts(f.t);
  var ts    = parts[0];
  var alpha = parts[1]; 

  assert( ts.length === ms.length , 
    'mkSexpr : lengths dont match' );
  
  var acc = f;

  for( var i = 0 ; i < ms.length ; i++ ){
    assert( _.isEqual( ts[i] , ms[i].t ) ,
      'mkSexpr : types are not equal' );
    
    acc = mkApp( acc , ms[i] );
  }

  return acc;
};

var mkTyp = function( x ){
  if( _.isArray(x) ){
    var ts  = _.map( x , mkTyp );
    var acc = ts[ts.length-1];
    for( var i = ts.length-2 ; i >= 0 ; i-- ){
      acc = mkArr(ts[i],acc);
    }
    return acc;
  }else{
    if(_.isString(x)){
      return mkAtm(x);  
    }else{
      return x;
    }
  }
};



var termSize = function(term,mode){
  assert( isTerm(term) , 'termSize : argument must be terms.' );
  switch(term.c){
    case VAR : return 1;
    case VAL : return 1;
    case APP : return (mode && mode.countAPPs ? 1 : 0) + termSize(term.m,mode) + termSize(term.n,mode);
    case LAM : return 1 + termSize(term.m,mode);
    case UNF : throw 'termSize : unfinished term';
    default  : throw 'termSize : default-in-switch error';
  }
};


var tParts = function(t){
  assert( isTyp(t) , 'tParts: t must be typ.' );
  var ts = [];
  while( isArr(t) ){
    ts.push( t.a );
    t = t.b; 
  }
  return [ ts , t ];
};

var tHead = function(t){
  assert( isTyp(t) , 'tHead: t must be typ.' );
  while( isArr(t) ){ t = t.b; }
  return t;
};





var mkCtx = function(obj){
  var ctx = {};
  for( var i in obj ){
    var name = i;
    var typ  = mkTyp(obj[i][0]);
    var fun  = obj[i][1];
    ctx[name] = { 
      t : typ, 
      f : fun 
    };
  }
  return ctx;
};

var mkAtmTab = function(ctx){
  var ret = {};
  for( var i in ctx ){ 
    var alpha  = tHead( ctx[i].t ).a;
    if( ret[alpha] === undefined ){ ret[alpha] = {}; }  
    ret[alpha][i] = mkVal(i,ctx[i].t);
  }  
  return ret;
};

var atmTabToCtx = function(tab){
  var ret = {};
  for(var i in tab){
    for( var j in tab[i] ){
      ret[j] = {
        t : tab[i][j].t  ,
        f : undefined
      };
    }
  }
  return ret;
};

var addVarToAtmTab = function(tab, newVar ){
  assert( isVar(newVar) , 'addVarToAtmTab : newVar must be var' );

  var ret   = _.clone(tab);
  var alpha = tHead(newVar.t).a;

  if( ret[alpha] === undefined ){ 
    ret[alpha] = {}; 
  } else {
    ret[alpha] = _.clone(ret[alpha]);
  } 

  ret[alpha][newVar.x] = newVar ;

  return ret;
};