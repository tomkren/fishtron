
var log = function(x){
  console.log(x);
};

var logArray = function(xs){
  _.each(xs,function(x){
      console.log(x);
  });  
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