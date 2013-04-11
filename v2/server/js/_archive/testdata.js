

/* Pokus s downloadem...... */

function onDownload() {
    document.location = 'data:text/plain,' +
                         encodeURIComponent(
'<?xml version="1.0" encoding="UTF-8"?>\n'+
'<kutil>\n'+
'  <object type="comment" val="abc" />\n'+
'  <object type="function" id="1" val="plus3" target="" pos="100 300" />\n'+
'  <object type="function" id="1_1" val="+" target="1:0" pos="100 200" />\n'+
'  <object type="function" id="1_1_1" val="id" target="1_1:0" pos="100 100" />\n'+
'  <object type="function" id="1_1_2" val="const 3" target="1_1:1" pos="200 100" />\n'+
'  <object type="function" id="1_2" val="inc" target="1:1" pos="200 200" />\n'+
'  <object type="function" id="1_2_1" val="id" target="1_2:0" pos="300 100" />\n'+
'  <object type="function" id="1_3" val="const 4" target="1:2" pos="300 200" />\n'+
'</kutil>'

                          );
}

//--------------------------------------------------------------------------


  var dummyGraph = [
    { node : 'a' , opts : {name:'foobar'}               } , 
    { node : 'b' , opts : {color:'red',name:'Int',sq:1} } ,  
    { node : 'c' , opts : {sq:1}                        } ,
    { node : 'd' , opts : {sq:1}                        } ,
    { node : 'e' , opts : {sq:1}                        } ,   
    
    { edge : ['a','b'] , opts : {dir:1,n:2}             } ,
    { edge : ['c','c'] , opts : {}                      } ,
    { edge : ['a','c'] , opts : {}                      } ,
    { edge : ['a','d'] , opts : {}                      } ,
    { edge : ['a','e'] , opts : {}                      } 
  ];

//--------------------------------------------------------------------------

var plotData_ = {
  g1 : function(d){for(var i=0;i<14;i+=0.5){d.push([i,Math.sin(i)]);}return d;}([]) ,
  g2 : [[0, 3], [4, 8], [8, 5], [9, 13]],
  g3 : [[0, 12], [7, 12], null, [7, 2.5], [12, 2.5]]
};


//--------------------------------------------------------------------------


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





