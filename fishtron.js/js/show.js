





var showAtmTab = function(tab,opt){
  return showCtx( atmTabToCtx(tab) , opt );
};

var showCtx = function(ctx,opt){
  var isShort = opt === 'short';
  var ret = isShort ? '' : '\n';
  for(var i in ctx){
     ret += i ; 
     ret += isShort ? ',' : 
            ' : '+code(ctx[i].t)+'\n' ;
  }
  ret = isShort ? ret.substr(0,ret.length-1) : ret ;
  return ret;
};






var evalTerms = function( terms , ctx ){
  var __termsStr = _.map(terms,toNicejs).join();
  var __result;
  with(ctxToWithobj(ctx)){
    eval('__result=[' + __termsStr + '];');
  }
  return __result;
};

// o něco pomalejší (cca 1.5×)
var evalTerms_2 = function( __terms , __ctx ){
  var __result;
  var __termsStr = '[' + _.map(__terms,toNicejs).join() + ']' ;
  eval( ctxToLocalcode(__ctx) +'__result='+ __termsStr + ';');
  return __result;
};

//cca 20× pomalejší !!!!
var evalTerms_slow = function( terms , ctx ){

  var ctxWithobj = ctxToWithobj(ctx);
  var __ret      = [];
  
  for( var i=0 ; i<terms.length ; i++ ){
    var __termStr = toNicejs(terms[i]);
    with(ctxWithobj){
      var __result;
      eval('__result=' + __termStr + ';');
      __ret.push(__result);
    }
  }

  return __ret;
};

var ctxToWithobj = function(ctx){
  var ret = _.clone(ctx);
  for( var i in ret ){
    ret[i] = ret[i].f;
  }
  return ret;
};

var ctxToLocalcode = function(ctx){
  var ret = '';
  for( var i in ctx ){
    ret += 'var '+ i + ' = ' + ctx[i].f.toString() + ';\n' ;
  }
  return ret;

};










//TODO asi smazat, divně je to řešený
var ctxCompile = function( m , ctx ){
  var ctxc = function(m){ 
    switch(m.c){
      case VAL : 
        if( ctx[m.x] ){
          return mkVal( ctx[m.x].f , ctx[m.x].t );
        }else{
          return m;
        } 
      case VAR : return m;
      case APP : return mkApp( ctxc(m.m) , ctxc(m.n) ) ;
      case LAM : 
        assert(isArr(m.t));
        return mkLam(m.x,m.t.a,ctxc(m.m));
    }
  }
  return code(ctxc(m));
};

var code = function(m,opt){
  if( isTyp(m) ){
    return typToStr(m);
  } else if( isZipper(m) ){
    return showZipper(m);
  } else if( _.isArray(m) ){
    return _.map(m,function(t){return code(t,opt);});      
  } else if( _.isObject(opt) ) {
    return ctxCompile(m,opt);
  } else {
    switch( opt ){
      case 'lc'       : return toLCstr     (m);
      case 'js'       : return toJSexpr    (m);
      case 'jsstr'    : return toJSstr     (m);
      case 'nicejs'   : return toNiceJSexpr(m);
      case 'nicejsstr': return toNicejs    (m);
      default         : return toNicejs    (m); //toJSexpr    (m);
    }
  }
};



var typToStr = function(t){
  switch(t.c){
    case ATM : return t.a; 
    case ARR : return '(' + typToStr(t.a) + '->' + typToStr(t.b) + ')';
    default  : throw 'Unsupported type: ' + t
  }  
};


var toJSexpr = function(m){
  var result;
  var jsStr = 'result=' + toJSstr(m) + ';';
  eval(jsStr);
  return result; 
};

var toNiceJSexpr = function(m){
  var result;
  var jsStr = 'result=' + toNicejs(m) + ';';
  eval(jsStr);
  return result;   
};

var toNicejs = function(m){
  switch(m.c){
    case VAL : return m.x; 
    case VAR : return m.x;
    
    case APP :
      var args = [];
      while(isApp(m)){
        args.unshift(toNicejs(m.n));
        m = m.m;
      }
      return toNicejs(m) + '(' + args.join(',') + ')';
    
    case LAM :
      var xs = [];
      while(isLam(m)){
        xs.push(m.x);
        m = m.m;
      }
      return 'function('+ xs.join()  +'){return '+ toNicejs(m) +'}';
    
    default : throw 'Unsupported term structure (toJSstr).'
  }
};

var toJSstr = function(m){
  switch(m.c){
    case VAL : return m.x; 
    case VAR : return m.x;
    case APP : return toJSstr(m.m) + '(' + toJSstr(m.n) + ')';
    case LAM : return 'function(' + m.x  + '){' +
                           'return ' + toJSstr(m.m) + ';}';
    default  : throw 'Unsupported term structure (toJSstr).'
  }
};


var toLCstr = function(m){
  switch(m.c){
    case VAL : return m.x; 
    case VAR : return m.x;
    case APP : return '( '+ toLCstr(m.m) +' '+ toLCstr(m.n) +' )';
    case LAM : return '( '+ m.x +' . '+ toLCstr(m.m) +' )';
    case UNF : return '[' + code(m.t) +';'+ showAtmTab(m.atmTab,'short')+']';
    default    : throw 'Unsupported term structure (toLCstr).'
  }
};






var showZipper = function(zipper){
  if( zipper === null ){ return '[NULL ZIPPER]'}
  assert( isZipper(zipper) , 'showZipper : it is not a zipper' );
  var show = function( zips , str ){
    if( !zips || !zips.head ){ return str; }
    switch( zips.head.c ){
      case 'lamZ'  :
        return show( zips.tail , 
          '( ' + zips.head.x + ' . ' + str + ')'  );
      case 'appLZ' :
        return show( zips.tail ,
          '( '+ str +' '+ code(zips.head.n,'lc') +' )' );
      case 'appRZ' : 
        return show( zips.tail ,
          '( '+ code(zips.head.m,'lc') +' '+ str +' )' );
    }
  };
  return show( zipper.zips , 
               ' { ' + code(zipper.act,'lc') + ' } ' ) + '   ['+ zipper.numUnfs +']';
};