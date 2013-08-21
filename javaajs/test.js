
var n  = 8;
var xs = [];
for(var i=0; i<n; i++){xs.push(i);}

var perms = function( xs ){
  if( xs.length === 0 ){ return [[]]; }
  var ret = [];
  for( var i = 0 ; i < xs.length ; i++ ){
    var x = xs[i];
    var rest = xs.slice(0,i).concat( xs.slice(i+1) );
    var ps = perms(rest);
    for( var j = 0 ; j < ps.length ; j++ ){
      ps[j].unshift(x);
    }
    ret = ret.concat( ps );
  }
  return ret;  
};

var fact = function(n){
  if(n === 0){return 1;}
  return n*fact(n-1);
};

println( perms(xs)[fact(n)-1] );

var _output_ = {
    y : function(x){ return x+_input_.i; } ,
    z : function(){return _input_.str + ' s přidanými vykřičníky!!!';} 
};