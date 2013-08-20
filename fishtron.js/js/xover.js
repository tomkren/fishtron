


// TODO NEXT : SKI conversion





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


