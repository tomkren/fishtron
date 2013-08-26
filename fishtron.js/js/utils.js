
var log = function(x){
  console.log(x);
};

var logArray = function(xs){
  if(!_.isArray(xs)){xs=[xs];}
  _.each(xs,function(x){
      log(x);
  });  
};

var logCodes = function(xs){
  return logArray(code(xs));  
};

var assert = function(condition, message) {
  if (!condition) {
    throw message || "Assertion failed!";
  }
};




var empty = null;
var cons = function(x,xs){
  return { head : x  , 
           tail : xs };
};



var partition = function(mustBeTrue,array){
  assert(_.isArray(array),'partition : the array argument mus be an array.');
  var satisfy    = [];
  var notSatisfy = [];
  for( var i = 0 ; i < array.length ; i++ ){
    if( mustBeTrue(array[i]) ){
      satisfy.push( array[i] );
    } else {
      notSatisfy.push( array[i] );
    }
  }
  return { satisfy    : satisfy ,
           notSatisfy : notSatisfy };
};




var mkTypeChecker = function(arg){
  return _.isArray(arg) ? 
         mkTypeChecker_union(arg) : 
         mkTypeChecker_const(arg) ;
};

var mkTypeChecker_const = function( typeConstructorConstant ){
  return function(x){
    return x.c === typeConstructorConstant ;
  };
};

var mkTypeChecker_union = function( typeCheckerFuns ){
  var len = typeCheckerFuns.length;
  return function(x){
    for( var i = 0 ; i < len ; i++ ){
      if(typeCheckerFuns[i](x)){
        return true;
      }
    }
    return false;
  };
};