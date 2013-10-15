// -- TSP test ---

$(function(){

  tsp1 = mkTSPInstance({
    'Praha,Londýn'  : 1034 ,
    'Praha,Berlín'  : 280  ,
    'Berlín,Londýn' : 929  ,
    'Berlín,Paříž'  : 876  ,
    'Paříž,Praha'   : 885  ,
    'Paříž,Londýn'  : 340
  });

  problem1 = {
    from     : 'Praha',
    fitness  : mkTSPFitness(tsp1,1000),
    heur     : mkTspHeur(tsp1),
    succsFun : mkTspSuccs(tsp1),
    initTau  : initTau(tsp1,1),
    isGoal   : mkTspIsGoal(tsp1),
    opts     : defaultOpts
  };

  test1 = step('Praha',problem1.initTau,problem1);
  test2 = aco( problem1 );

});


// --- obecný mravenci -------------------

var defaultOpts = {
  rho          : 0.15 ,
  alpha        : 1.2  ,
  beta         : 1.2  ,
  antsPerRound : 100  ,
  numRounds    : 10   ,
};

function aco( problem ){

  var best = initBest;
  var tau  = problem.initTau;

  for(var i = 0 ; i < problem.opts.numRounds ; i++){
    var stepResult = step( problem.from, tau , problem );
    tau  = stepResult.tau;
    best = updateBest(best,stepResult.best); 
  }

  return best;
}

var initBest = {
  path   : null,
  fitVal : 0
};

function updateBest( oldRes , newRes ){
  if( oldRes.fitVal < newRes.fitVal ){
    return newRes;
  }
  return oldRes;
}

function step( from , tau , problem ){

  var succsFun = problem.succsFun;
  var heur     = problem.heur;
  var isGoal   = problem.isGoal;
  var fitness  = problem.fitness;
  var opts     = problem.opts;

  var antPaths = [];

  for( var i = 0; i < opts.antsPerRound; i++ ){
    var path = mkPath( from , tau , succsFun , heur , isGoal , opts );
    if(path !== null ){
      antPaths.push(path);
    }
  }

  return updateTau( tau , antPaths , fitness , opts );
}


function updateTau( oldTau , antPaths , fitness , opts ){

  var newTau = {};

  var i,j;

  for( i in oldTau ){
    if( newTau[i] === undefined ){ newTau[i] = {}; }
    for( j in oldTau ){
      newTau[i][j] = oldTau[i][j] * (1-opts.rho); 
    }
  }

  var best = initBest;

  for( var k = 0 ; k < antPaths.length ; k++ ){
    var path    = antPaths[k];
    var fitVal  = fitness(path);

    best = updateBest(best,{path:path,fitVal:fitVal});

    var pathLen = antPaths[k].length;
    for( var s = 0 ; s < pathLen-1 ; s ++ ){

      i = antPaths[k][s];
      j = antPaths[k][s+1];

      newTau[i][j] += fitVal ;
    }

  }

  return {
    tau  : newTau, 
    best : best
  };
}



function initTau( obj , val ){
  var tau = {};
  var i,j;
  for( i in obj ){
    if( tau[i] === undefined ){ tau[i] = {}; }
    for( j in obj[i] ){
      tau[i][j] = val; 
    }
  }
  return tau;
}


function selectSucc( from , path , tau , succsFun , heur , opts ){

  var ss = succsFun(from,path);

  if( ss.length === 0 ){ return null; }

  var ps = [];

  var pSum = 0;
  var i;

  for( i = 0 ; i < ss.length ; i++ ){
    var to = ss[i];
    var probab = Math.pow( tau[from][to], opts.alpha) * Math.pow( heur(from,to), opts.beta);
    ps.push( probab );
    pSum += probab;
  }

  var ball = Math.random() * pSum;
  var nowSum = 0;

  for( i = 0 ; i < ps.length ; i++ ){
    nowSum += ps[i];
    if( ball < nowSum ){
      break;
    }
  }

  return ss[i]; //[ss[i],i,ball,ps,ss,pSum];
}

function mkPath( from , tau , succsFun , heur , isGoal , opts ){
  var next = from;
  var path = [];

  while( !isGoal(path) && next !== null ){

    path.push(next);
    var next = selectSucc( next , path , tau , succsFun , heur , opts );  
    
  }

  return isGoal(path) ? path : null ;

  
}



// ------ TSP --------------


function mkTspHeur( tsp ){
  return function(i,j){
    if( tsp[i][j] === undefined ){ return 0; }
    return 1 / tsp[i][j] ; 
  }
}

function mkTspSuccs( tsp ){

  return function(from,path){
    var ret = [];

    for( var to in tsp[from] ){

      var obsahuje = false;
      for(var s = 0; s < path.length ; s++){
        if( path[s] === to){
          obsahuje = true;
        }
      }

      if(!obsahuje){
        ret.push(to);
      }

    }

    return ret;
  };
}

function mkTspIsGoal( tsp ){
  var size = _.size(tsp);
  return function(path){
    return path.length === size ;
  }
}

function mkTSPInstance(obj){

  var graph = {};

  for( var prop in obj ){    
    var ps = prop.split(',');

    var i = ps[0];
    var j = ps[1];

    if( graph[i] === undefined ){
      graph[i] = {};      
    }
    graph[i][j] = obj[prop];

    if( graph[j] === undefined ){
      graph[j] = {};      
    }

    if( graph[j][i] === undefined ){
      graph[j][i] = graph[i][j];  
    }

  }

  return graph;
}





function mkTSPFitness(tsp,Q){
  return function(path){
    var sum = 0;
    for( var s = 0 ; s < path.length-1 ; s++ ){
      var i = path[s];
      var j = path[s+1];
      sum += tsp[i][j];
    }

    sum += tsp[ path[path.length-1] ][ path[0] ];

    return Q / sum;
  };
}



