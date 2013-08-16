var PriorityQueue = function(){
  
  var interfaceObj;
  var queue = [];
 
  var push = function( x , val ){
    var newEntry = [x,val];
    var i = _.sortedIndex(queue,newEntry,function(pair){return pair[1];});
    queue.splice(i,0,newEntry);
    return interfaceObj;
  }
  
  var pop = function(){
    return queue.shift();
  }
  
  var isEmpty = function(){
    return queue.length === 0;
  };
  
  var toString = function(showFun){

    if( isEmpty() ){
      return '[EMPTY PRIORITY QUEUE]';
    }

    if(showFun === undefined){
      showFun = function(x){return x;}; 
    }
  
    return _.map(queue,function(pair){
      return showFun(pair[0]) +' ('+ pair[1] + ')'  ;
    }).join('\n');
  };


  interfaceObj = {
    push     : push,
    pop      : pop,
    isEmpty  : isEmpty,
    toString : toString,
  };

  return interfaceObj;
};