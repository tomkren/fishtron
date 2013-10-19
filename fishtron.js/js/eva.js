$(function(){

});

var int = mkAtm('int');

var GPOpts1 = {
  fitness : function(x){return 42;},
  typ     : mkTyp([int,int]),
  ctx     : mkCtx({
    'plus' : [ [int,int,int]
           , function(x,y){return x+y;} ],
    'sin'  : [ [int,int]
           , Math.sin ] 
  }), 
  popSize : 500,
  numGens : 51,
  probabs : {
    crossover    : 0.9,
    reproduction : 0.1,
    mutation     : 0.0
  }
};


function gp(opts){

  var gen = 0;
  var pop = prove({
    n          : opts.popSize,
    typ        : opts.typ,
    ctx        : opts.ctx,
    resultMode : 'funs',
    logit      : false 
  });



  return pop;
}









