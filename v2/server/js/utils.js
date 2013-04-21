var curr = function( fun ){
  return curryfy( fun.length , fun , [] );
};

var curryfy = function( ii , fun , args ){

  if( ii == 0 ){
    return fun.apply( null , args );
  }

  return function(){

    var args2 = args;
    for (var j = 0; j < arguments.length; j++) {
      args2 = args2.concat( [arguments[j]] );
    }

    return curryfy( ii-arguments.length , fun , args2 );
  };

};


var curt = function( fun ){
  var origNumArgs = fun.length ;
  return function(){
    console.log( "num args original: " + origNumArgs );
    console.log( "num args applied : " + arguments.length );
    console.log( "arguments:         " + arguments );

    return [fun.apply( null , arguments ),arguments];
  };
}

var argsToArr = function( args ){
    ret = [];
    for (var j = 0; j < arguments.length; j++) {
      args2 = args2.concat( [arguments[j]] );
    }  
}

var te1 = function( a , b , c ){
  return a + b + c;
};


var cur2 = function( fun ){
  return function( x , y ){
    if( y === undefined ){
      return function(y){ return fun(x,y) ; }
    } else {
      return fun( x , y );
    }
  }
}; 

var cur3 = function( fun ){
  return function( x , y , z ){
    if( y === undefined ){
      return cur2( function(y,z){return fun(x,y,z);} );
    } else if( z === undefined ){
      return function(z){return fun(x,y,z);} ;
    } else {
      return fun(x,y,z);
    }
  }
};

var plus2 = cur2(function(x,y){
  return x + y;
});

var plus3 = cur3(function(x,y,z){
  return x + y + z;
});



//var k = _.partial(curr( function( x , y ){
//  return x;
//});

var slice = Array.prototype.slice ;

var ap = function(func) {
    var args = slice.call(arguments, 1);
    return function() {
      return func.apply(this, args.concat(slice.call(arguments)));
    };
  };




var not = function( x ){
  return (!x) ;
};

var and = cur2(function( x , y ){
  return (x && y) ;
});

var or = cur2(function( x , y ){
  return (x || y) ;
});

var nand = cur2(function( x , y ){
  return ! (x && y) ;
});

var nor = cur2(function( x , y ){
  return ! (x || y) ;
});


var k = cur2(function( x , y ){
  return x;
});

var s = cur3( function( x , y , z ){
  return x(z)(y(z));
});

var i = function( x ){
  return x;
};


var cons = cur2(function( x , xs ){
  return [x].concat(xs);
});

var foldr = cur3(function( f , z , xs ){

  var ret = z;
  for( var i = xs.length-1 ; i >= 0 ; i-- ){
    ret = f(xs[i])(ret);
  } 
  return ret;

});

//listCase :: [a] -> b -> (a->[a]->b) -> b

var listCase = cur3(function(list,emptyCase,fullCase){
  if( list.length == 0 ){
    return emptyCase ;
  } else {
    return fullCase (list[0]) (list.splice(1));
  }
});

var Just = function(x){
  return [x];
};

var Nothing = [];
var True    = true;
var False   = false;

var if_ = cur3(function(p,q,r){
  if(p){
    return q;
  } else {
    return r;
  }
});

var equals = cur2(function(x,y){
  return x == y ;
});

var notEquals = cur2(function(x,y){
  return x != y ;
});


var foo  = function (x0,x1){return foldr(  s ( k(cons) ,  s(k(x0),i)   )   , [] , x1 );}

var foo2 = function (x0,x1){return foldr(  s ( k(cons) )(  s(k(x0))(i)   )   )( [] )( x1 );}

var plus1 = function(x){ return x+1 ; };

// -- SSR --------------------------------------------------




var sin  = Math.sin ;
var cos  = Math.cos ;
var exp  = Math.exp ;
var rdiv = function(x,y){if(y==0){return 1;}else{return x/y;}}
var rlog = function(x){return Math.log(Math.abs(x));}


var ssr = function( solution ){

  var trueSolution = function(x){return x*x*x*x+x*x*x+x*x+x ;}

  var xs = [ ] ; 
  for (var i = -1 ; i <= 1 ; i+= 0.01 ) {
    xs.push(i);
  }

  ysSol     = _.map( xs , function(x){return [x,    solution(x)];} );
  ysTrueSol = _.map( xs , function(x){return [x,trueSolution(x)];} );

  
  var ssrPlotData = {
    trueSolution : ysTrueSol,
    solution     : ysSol
  };

  $('#special').html(
    'Best individual : ' +
    '<div id="graphssr-div" class="graph"></div>');

  drawPlot( 'ssr' , ssrPlotData );


  return ssrPlotData;

};








