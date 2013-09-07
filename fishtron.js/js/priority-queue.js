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
    toString : toString
  };

  return interfaceObj;
};

var Heap = function(){
  
  var interfaceObj;
  var heap = [null];

  var size = function(){
    return heap.length - 1;
  };

  var f = function(i){
    return heap[i][1];
  };

  var isRoot = function(i){
    return i === 1 ;
  };

  var isLeaf = function(i){
    return isOut(lSon(i)); 
  };

  var isOut = function(i){
    return i > size();
  };

  var swap = function(i,j){
    var temp = heap[i];
    heap[i]  = heap[j];
    heap[j]  = temp;
  };

  var lSon = function(i){
    return 2*i;
  };

  var rSon = function(i){
    return 2*i+1;
  };

  var minSon = function(i){
    var left  = lSon(i);
    var right = rSon(i);

    if(isOut(right)){return left;}

    return f(left) <= f(right) ? left : right;
  }

  var dad = function(i){
    return Math.floor(i/2);
  };

  var up = function(i){
    while( !isRoot(i) && f(i) < f(dad(i)) ){
      swap(i,dad(i));
      i = dad(i);
    }
  };

  var down = function(i){
    if(isLeaf(i)){return;}
    
    var son = minSon(i);
    while( !isLeaf(i) && f(son) < f(i) ){
      swap(son,i);
      i   = son;
      son = minSon(i);
    }
  };


 
  var push = function( x , val ){
    var len = heap.push([x,val]);
    up(len-1);
    return interfaceObj;
  };
  
  var pop = function(){
    if(isEmpty()){return undefined;}
    swap(1,heap.length-1);
    var top = heap.pop();
    down(1);
    return top;
  };
  
  var isEmpty = function(){
    return size() === 0;
  };
  
  var toString = function(showFun){

    if( isEmpty() ){
      return '[EMPTY PRIORITY QUEUE]';
    }

    if(showFun === undefined){
      showFun = function(x){return x;}; 
    }
  
    var arrToPrint = _.sortBy(_.rest(heap),function(pair){
      return pair[1];
    });

    return _.map( arrToPrint ,function(pair){
      return showFun(pair[0]) +' ('+ pair[1] + ')'  ;
    }).join('\n');
  };

  var showHeap = function(){
    var h = 0;
    var ret = '';
    for(var i = 1 ; i < heap.length ; i++){
      ret += heap[i][1] + ' ' ;
      if( i === (Math.pow(2,h+1)-1) ){
        ret += '\n';
        h++;
      }
    }
    log(ret);
  };


  interfaceObj = {
    push     : push,
    pop      : pop,
    isEmpty  : isEmpty,
    toString : toString,
    showHeap : showHeap
  };

  return interfaceObj;
};