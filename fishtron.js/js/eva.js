$(function(){

});

var GPOpts1 = {
  fitness : function(x){return 42;},
  typ     : mkTyp(['a','a']),
  ctx     : mkCtx({}), 
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
    resultMode : 'funs'
  });

  throw 'TODO';
}

