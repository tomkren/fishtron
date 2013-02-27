
var cmd1 = {
  type : 'stdout',
  msg  : 'Hello vesmíre!'
};


var cmd2 = function(x,y){
  return {
  type  : 'graph' ,
  graph : 'g4' ,
  data  : [x,y] 
};};

var cmd3 = {
  type : 'multi',
  cmds : [
    cmd1 ,
    cmd2(10,8) , 
    { type : 'stdout', msg : 'Kolem tvé vlastní osy!' } ,
    { type : 'stdout', msg : 'Hnojopražce..?' } ,   
    cmd2(11,7), 
    cmd2(12,16)
  ]
};

var cmd4 = {
  type   : "generationInfo",
  i      : 0,
  ffvals : {
    best  : 0.56473324 ,
    avg   : 0.26871469 ,
    worst : 0.11201199 
  }
};

var cmd5 = {
  type   : "generationInfo",
  i      : 1,
  ffvals : {
    best  : 0.79114577 ,
    avg   : 0.67221454 ,
    worst : 0.09771199 
  }
};

var cmd6 = {
  type   : "generationInfo",
  i      : 2,
  ffvals : {
    best  : 0.79914577 ,
    avg   : 0.68221454 ,
    worst : 0.19771199 
  }
};

var cmd7 = {
  type : 'multi',
  cmds : [cmd4,cmd5,cmd6]
};





