
var ATM    = 'atm'; // 1;
var ARR    = 'arr'; // 2;
                            
var VAR    = 'var'; // 3;
var VAL    = 'val'; // 4;
var APP    = 'app'; // 5;
var LAM    = 'lam'; // 6;
var UNF    = 'unf'; // 7;
                     
var ZIPPER = 'zipper'; // 8;



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


var checkTerms = function(terms,typ,ctx){
  var result;
  var ok = true;
  for( var i = 0 ; i < terms.length ; i++ ){
    result = checkTerm(terms[i],typ,ctx);
    if( !result.ok ){
      result.koTerm = terms[i];
      ok = false;
      break;
    }
  }
  if(ok){ result.msg = 'checkTerms : All terms are OK!' };
  return result;
};

var checkTerm = function(term,typ,ctx){ // TODO z venčí by měl bejt danej eště typ co je čeknut

  var ctxSupplied = (ctx !== undefined);

  var checkOK  = {ok:true,msg:'Term is OK!'};
  var checkErr = function(msg){return{ok:false,msg:msg};};

  if( typ !== undefined && !_.isEqual(typ,term.t) ){
    return checkErr('high-level-typ-mismatch');
  }

  var check = function(term,baze){

    if( ! isTerm(term) ){
      return checkErr('unsupported term constructor');
    }

    switch(term.c){
      case VAR : 

        if( baze[term.x] === undefined ){
          return checkErr('undefined VAR ' + term.x + ' is used' );
        }
        if( !_.isEqual(baze[term.x],term.t) ){
          return checkErr('(var,baze)-typ-mismatch in VAR ' + term.x ); 
        }

        return checkOK;        

      case VAL : 

        if( ctxSupplied ){
          if( ctx[term.x] === undefined ){
            return checkErr('supplied ctx has no VAL ' + term.x );
          }
          if( !_.isEqual(ctx[term.x].t,term.t) ){
            return checkErr('(val,ctx)-typ-mismatch in VAL ' + term.x ); 
          }
        }

        return checkOK;

      case LAM : 

        if( !isArr(term.t) ){
          return checkErr('LAM must have arrow typ, but LAM with var '+term.x+' has typ '+code(term.t) );
        }
        if( ctxSupplied && ctx[term.x] !== undefined ){
          return checkErr('LAM shadows VAL '+term.x+' from ctx by its var');
        }
        if( baze[term.x] !== undefined ){
          return checkErr('LAM shadows VAR '+term.x);
        }

        if( !_.isEqual(term.t.b,term.m.t) ){
          return checkErr('typ mismatch in ' + term.x );
        }

        baze[term.x] = term.t.a;
        var subResult = check(term.m,baze);
        baze[term.x] = undefined;

        return subResult;


      case APP : 

        var mTyp = term.m.t;  //        ....A->B
        var nTyp = term.n.t;  //        ... A
                              // term.t ... B

        if( !isArr(mTyp) ){
          return checkErr('m-term of APP must have arrow type');
        }
        if( !_.isEqual( mTyp.a , nTyp ) ){
          return checkErr('(A->B,A)-typ-mismatch in APP');
        }
        if( !_.isEqual( mTyp.b , term.t ) ){
          return checkErr('(A->B,B)-typ-mismatch in APP');
        }

        var mResult = check(term.m,baze);

        if( !mResult.ok ){
          return mResult;
        }

        return check(term.n,baze);

      case UNF : return checkErr('UNF is not allowed');
      default  : return checkErr('metaERROR this should be unreachable (1)');
    }

    return checkErr('metaERROR this shuold be unreachable (2)');
  };

  var baze   = {};
  var result = check(term,baze);
  result.msg = 'checkTerm : ' + result.msg;

  return result;
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