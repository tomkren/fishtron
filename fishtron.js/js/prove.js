var prove = function(opts){
  if(opts === undefined){
    opts = {logit:true};
  } 

  var typ      = opts.typ      || mkTyp(['a','a']);
  var ctx      = opts.ctx      || mkCtx({});
  var evalThem = opts.evalThem || false;

  var ret = treeAStar({
    n     : opts.n     || 1,
    limit : opts.limit || 1000000,
    start : mkStartZipperSmart(typ,ctx),
    nexts : smartExpand,
    isGoal: function(zipper){return zipper.numUnfs === 0 ;},
    heur  : function(zipper){return zipper.numUnfs;}
  });  

  ret = _.map(ret,function(z){return gotoTop(z).act;});

  if( opts.logit ){
    _.each(ret,function(term,i){
      log( (i+1) + ' : \t' + code(term,'nicejsstr') );
    });  
  }

  if(evalThem){
    ret = evalTerms(ret,ctx);
  }

  return ret;
};


var treeAStar = function(problem){

  var start  = {s:problem.start,G:0} ;
  var nexts  = problem.nexts  ;
  var isGoal = problem.isGoal ;
  var n      = problem.n      || 1;
  var heur   = problem.heur   || function(x){return 0;};
  var limit  = problem.limit  || 1000000;

  var q = PriorityQueue(); 
  var results = [];

  q.push( start , heur(start.s) );

  while( !q.isEmpty() && limit > 0 ){
      
    limit--;

    var st    = q.pop()[0];
    var state = st.s    ;
    var G     = st.G    ;
    //log( 'vyndavam : ' + showZipper(state) );

    if( isGoal(state) ){
      results.push(state);
      if( results.length == n ){
        return results;
      }
    } else { // TODO : obecně to ale pro grafovej Astrar neplatí
             // (to že je to v else) prorože i za cílovym může bejt 
             // další cílovej, u nás toé ale tak rozhodně neni a tak si to dovolíme
             // vlastně se jedná o treeAStar-s-goalama-v-listech-tho-tree

      var ns = nexts(state);
      for( var i = 0 ; i < ns.length ; i++ ){
        
        var newG = G + ns[i].dist ;
        var next = {s : ns[i].state ,
                    G : newG  };
        
        //log( '  vkladam : ' + showZipper(ns[i].state) );
        q.push( next , newG + heur(next.s) ); 
      }
    }
  }
  return results;
};



/* /
$(function(){


  sumStr = function(str){
    var sum = 0;
    for( var i = 0 ; i < str.length ; i++ ){
      sum += parseInt(str[i]);
    }
    return sum;
  };

  res = treeAStar({
    n     : 100 ,
    start : '' ,
    isGoal: function(x){
              return sumStr(x) == 10;} ,
    nexts : function(x){
              return [ {state:x+'0',dist:1} ,
                       {state:x+'1',dist:1} ]; },
    heur  : function(x){
              return Math.max( 0 , 10 - sumStr(x) );}
  });  

});
/ */