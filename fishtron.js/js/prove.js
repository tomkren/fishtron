var prove = function(opts){
  if(opts === undefined){
    opts = {logit:true};
  } 

  var typ        = opts.typ        || mkTyp(['a','a']);
  var ctx        = opts.ctx        || mkCtx({});
  var resultMode = opts.resultMode || 'terms';

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

  switch( resultMode ){
    case 'terms': return ret;
    case 'funs' : return evalTerms(ret,ctx);
    case 'both' : return [ret,evalTerms(ret,ctx)];
    default     : throw 'prove : unsupported result mode.'
  }

};

var mkStartZipperSmart = function(t,ctx){
  var zipper = mkZipper({ 
        act     : mkUnf(t, mkAtmTab(ctx) ) ,
        zips    : empty,
        numUnfs : 1  
  });

  if( isArr(t) ){
    zipper = gotoNextUnf(expandLam(zipper).zipper);
  }

  return zipper;
};


var smartExpand = function( zipper ){
  assert( isUnf(zipper.act) , 'smartExpand : act must be unf to be smart-expanded'+
  '\n zipper : '+ showZipper(zipper) );

  var typ    = zipper.act.t;
  var atmTab = zipper.act.atmTab;

  assert( isAtm(typ), 'smartExpand : Smart-expanded can be only atomic type.' );

  var ret = [];

  var row = atmTab[typ.a];
  if( row !== undefined ){
    for( var i in row ){

      var varI = zipper.nextVar; 

      var ms = _.map( tParts(row[i].t)[0] , function(t){

        var newAtmTab = atmTab;   
        var newVars   = [];

        while( isArr(t) ){
          var newVar = mkVar( '_'+varI , t.a );
          newVars.push(newVar);
          newAtmTab = addVarToAtmTab(newAtmTab,newVar);
      
          varI ++;
          t = t.b;
        }
      
        var acc = mkUnf(t,newAtmTab);
      
        for( var i = newVars.length-1 ; i >= 0 ; i-- ){
          acc = mkLam( newVars[i] , acc );
        }

        return acc;
      });
      
      var newNumUnfs = zipper.numUnfs - 1 + ms.length;

      var newZipper  = mkZipper({
                          act     : mkSexpr(row[i],ms),
                          numUnfs : newNumUnfs, 
                          nextVar : varI  
                       },zipper);

      if( newNumUnfs !== 0 ){
        newZipper = gotoNextUnf( newZipper );
      }


      ret.push({
        state : newZipper,
        dist  : 1 + (varI-zipper.nextVar) 
      });

    }
  }

  return ret;

};


// TODO dost podobnej kód je ve smart expand, nějak to 
// zredukovat do jednoho ( aby to neduplikovalo znalost ! :) )
var expandLam = function( zipper ){
  assert( isUnf(zipper.act) , 
          'act must be unf to be lam-expanded' );

  var t         = zipper.act.t;
  var newAtmTab = zipper.act.atmTab; 
  var varI      = zipper.nextVar; 
  var newVars   = [];


  assert( isArr(t), 'Lam-expanded can be only arrow type.' );

  
  while( isArr(t) ){
    var newVar = mkVar( '_'+varI , t.a );
    newVars.push(  newVar  );
    newAtmTab = addVarToAtmTab( newAtmTab , newVar );

    varI ++;
    t = t.b;
  }

  var acc = mkUnf( t , newAtmTab );

  for( var i = newVars.length-1 ; i >= 0 ; i-- ){
    acc = mkLam( newVars[i] , acc );
  }

  return {
    zipper : mkZipper({
               act     : acc,
               nextVar : varI 
             },zipper),
    dist   : newVars.length 
  };  
};




var treeAStar = function(problem){

  var start  = {s:problem.start,G:0};
  var nexts  = problem.nexts;
  var isGoal = problem.isGoal;
  var n      = problem.n      || 1;
  var heur   = problem.heur   || function(x){return 0;};
  var limit  = problem.limit  || 1000000;

  var q = PriorityQueue(); 
  var results = [];

  q.push( start , heur(start.s) );

  while( !q.isEmpty() && limit > 0 ){
      
    limit--;

    var st    = q.pop()[0];
    var state = st.s;
    var G     = st.G;
    //log( 'vyndavam : ' + showZipper(state) );

    if( isGoal(state) ){
      results.push(state);
      if( results.length === n ){
        return results;
      }
    } else { // TODO : obecně to ale pro grafovej Astrar neplatí
             // (to že je to v else) prorože i za cílovym může bejt 
             // další cílovej, u nás toé ale tak rozhodně neni a tak si to dovolíme
             // vlastně se jedná o treeAStar-s-goalama-v-listech-tho-tree

      var ns = nexts(state);
      for( var i = 0 ; i < ns.length ; i++ ){
        
        var newG = G + ns[i].dist;
        var next = {s : ns[i].state ,
                    G : newG  };
        
        //log( '  vkladam : ' + showZipper(ns[i].state) );
        q.push( next , newG + heur(next.s) ); 
      }
    }
  }
  return results;
};



/* 
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


var expandApp = function( zipper ){
  assert( isUnf(zipper.act) , 
          'act must be unf to be app-expanded' );

  var typ    = zipper.act.t;
  var atmTab = zipper.act.atmTab;

  assert( isAtm(typ), 'App-expanded can be only atomic type.' );

  var ret = [];

  var row = atmTab[typ.a];
  if( row !== undefined ){
    for( var i in row ){

      var ms = _.map( tParts(row[i].t)[0] , function(t){
        return mkUnf(t,atmTab);
      });
      
      ret.push(mkZipper({
        act     : mkSexpr(row[i],ms),
        numUnfs : zipper.numUnfs - 1 + ms.length 
      },zipper));

    }
  }

  return ret;

};

*/