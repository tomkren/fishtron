


// TODO NEXT : pořádně otestovat AE !!!!!!!!!!!!!!!!!!


//S( K(  S(  S(K(p) , S( K(ap42) , S(K(K),I)  )  )) )  , S(K(K),I)    )

var S = function(f,g){
  return function(x){
    return f(x)(g(x));
    //_partial(_.partial(f,x),_.partial(g,x)); //taky blbě pač _.partial(f_1arg,42) je fce co čekí na ()
  };
};

var K = function(x){
  return function(y){
    return x;
  };
};

var I = function(x){
  return x;
};

//var cur2 = function(f){
//  return function(x){
//    return function(y){
//      return f(x,y);
//    };
//  };
//};



var FV = function(term){
  switch( term.c ){
    case VAL : return [];
    case VAR : return [term.x];
    case APP : return _.union( FV(term.m) , FV(term.n) );
    case LAM : return _.without( FV(term.m) , term.x );
    default  : throw 'FV : default in switch';
  }
};


var AE = function(term){
  switch( term.c ){
    case VAL : 
    case VAR : return term;
    case APP : return mkApp( AE(term.m) , AE(term.n) );
    case LAM :
       
      if( isVar(term.m) && term.m.x === term.x ){
        return mkVal('I',term.t);
      }
         
      var mFV = FV(term.m);
        
      if( !_.contains(mFV,term.x) ){
        // K : B -> (A -> B)
        return mkApp( mkVal('K', mkArr( term.t.b , term.t ) ) ,  AE(term.m) );
      }else{
        
        if( isLam(term.m) ){
          return AE( mkLam_(term.x, AE(term.m), term.t) ); 
        }

        if( isApp(term.m) ){

          var xTyp = term.t.a;

          var lam1 = AE(mkLam(term.x,xTyp,term.m.m));
          var lam2 = AE(mkLam(term.x,xTyp,term.m.n));

          //S : (A->B->C) -> (A->B) -> a -> c
          //S f g x = f x (g x)
          var sTyp = mkTyp([ lam1.t , lam2.t , term.t ]);
          
          return mkApp( mkApp( mkVal('S',sTyp), lam1 ) , lam2 );
        }

        throw 'AE : this place in code should be unreachable';        
      }
    default : throw 'AE : default in switch';
  }
};





var isWayToLeaf = function(way){
  return way.isLeaf ;
};

var allWays = function( term , mode ){

    var isAtTreeMode = !mode || mode !== 'sexprTree';

    var allWays_ = function( term , wayToTerm ){
      switch(term.c){
        case VAR : return [{way : wayToTerm, t: term.t, isLeaf: true}];
        case VAL : return [{way : wayToTerm, t: term.t, isLeaf: true}];
        case APP : 
          if( isAtTreeMode ){
            var mWays = allWays_( term.m , wayToTerm.concat(['m']) );
            var nWays = allWays_( term.n , wayToTerm.concat(['n']) );
            return [{way : wayToTerm, t: term.t, isLeaf: false}].concat(mWays).concat(nWays);    
          } else { //sexprTreeMode
            var ret = [];
            var accWay = wayToTerm;
            while( isApp(term) ){
                ret = allWays_( term.n , accWay.concat(['n']) ).concat(ret);
                accWay = accWay.concat(['m']);
                term = term.m;
            }
            return [{way : wayToTerm, t: term.t, isLeaf: false}].concat( ret );            
          }
        case LAM : 
          var mWayz = allWays_( term.m , wayToTerm.concat(['m']) );
          return [{way : wayToTerm, t: term.t, isLeaf: false}].concat(mWayz);
        case UNF : throw 'allPoses : UNF in switch'
        default  : throw 'allPoses : default in switch '
      }
    };
    
    return allWays_(term,[]);
};



var subterm = function( term , wayToSubterm ){
  for( var i=0 ; i<wayToSubterm.way.length ; i++ ){
    term = term[wayToSubterm.way[i]];
  }
  return term;
};


var changeSubterm = function( term , way , newSubterm ){

  assert( _.isEqual(way.t,newSubterm.t) , 'changeSubterm : way typ and newSubterm typ do not match.' );

  var w = way.way;
  var zipper = mkZipperFromTerm(term);
  var go = {  
    m : goM,
    n : goN
  };

  for( var i = 0 ; i < w.length ; i++ ){
    zipper = go[w[i]](zipper);
  }

  var oldSubterm = zipper.act ;

  assert( _.isEqual(way.t,oldSubterm.t) , 'changeSubterm : way typ and oldSubterm typ do not match.' );

  zipper = mkZipper({act : newSubterm} , zipper);
  zipper = gotoTop(zipper);

  return {
    newTerm     : zipper.act ,
    oldSubterm  : oldSubterm
  };

};


