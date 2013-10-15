
// --- obecný mravenci -------------------

var defaultOpts = {
  rho : 0.15 ,

};


function updateFeroMap( feroMap , antPaths , fitness , opts ){

  var ret = {};


  for( var prop in feroMap ){
    ret[prop] = feroMap[prop] * (1-opts.rho); 
  }


  for( var k = 0 ; k < antPaths.length ; k++ ){

    var pathLen;
    var fitVal = fitness(antPaths[k]);

    for( var s = 0 ; antPaths[k].length-1 ; s ++ ){

      var i = antPaths[k][s];
      var j = antPaths[k][s+1];

      ret[ij(i,j)] += fitVal ;

    }

  }

  return ret;

  //return (1-opts.rho)*oldTau + sum(deltaTaus); 

}

function ij(i,j){
  return i+','+j ;
}





// ------ TSP --------------

var tspInstance = mkTSPInstance({
  'Praha,Londýn'  : 1034 ,
  'Praha,Berlín'  : 280  ,
  'Berlín,Londýn' : 929  ,
  'Berlín,Paříž'  : 876  ,
  'Paříž,Praha'   : 885  ,
  'Paříž,Londýn'  : 340
});

function mkTSPInstance(obj){
  var ret = {};
  for( var prop in obj ){
    ret[prop] = obj[prop]; 
    
    var ps = prop.split(',');
    var revProp = ps[1] +','+ ps[0];
    if(obj[revProp] === undefined){
      ret[revProp] = obj[prop];
    }
  }
  return ret;
}





function mkTSPFitness(tsp){
  return function(path){

  };
}






