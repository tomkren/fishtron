

// ????????????????????????????????????????????????????????????????????????????????????
// TODO :  ??? jsou ubec ty cesty potřeba, nestačí přirozeně pracovat s podstromama ???
// ????????????????????????????????????????????????????????????????????????????????????




var tt1 = {"c":6,"x":"_0","m":{"c":6,"x":"_1","m":{"c":5,"m":{"c":5,"m":{"c":4,"x":"p","t":{"c":2,"a":{"c":1,"a":"int"},"b":{"c":2,"a":{"c":1,"a":"int"},"b":{"c":1,"a":"int"}}}},"n":{"c":5,"m":{"c":4,"x":"ap42","t":{"c":2,"a":{"c":2,"a":{"c":1,"a":"int"},"b":{"c":1,"a":"int"}},"b":{"c":1,"a":"int"}}},"n":{"c":6,"x":"_2","m":{"c":3,"x":"_1","t":{"c":1,"a":"int"}},"t":{"c":2,"a":{"c":1,"a":"int"},"b":{"c":1,"a":"int"}}},"t":{"c":1,"a":"int"}},"t":{"c":2,"a":{"c":1,"a":"int"},"b":{"c":1,"a":"int"}}},"n":{"c":3,"x":"_0","t":{"c":1,"a":"int"}},"t":{"c":1,"a":"int"}},"t":{"c":2,"a":{"c":1,"a":"int"},"b":{"c":1,"a":"int"}}},"t":{"c":2,"a":{"c":1,"a":"int"},"b":{"c":2,"a":{"c":1,"a":"int"},"b":{"c":1,"a":"int"}}}}


var tt2 = mkVar('x');

var allWays = function( term , wayToTerm ){

  if( wayToTerm === undefined ){ wayToTerm = []; }

  switch(term.c){
    case VAR : return [wayToTerm] ;
    case VAL : return [wayToTerm] ;
    case APP : 
      var mWays = allWays( term.m , ['m'].concat(wayToTerm) );
      var nWays = allWays( term.n , ['n'].concat(wayToTerm) );
      return [wayToTerm].concat(mWays).concat(nWays);
    case LAM : 
      var mWays = allWays( term.m , ['m'].concat(wayToTerm) );
      return [wayToTerm].concat(mWays);
    case UNF : throw 'allPoses : UNF in switch'
    default  : throw 'allPoses : default in switch '
  }

};

var allWays_mustBeTrue = function( term ){
  return allWays(term).length === termSize(term,{countAPPs:true});
};

var subterm = function( term , wayToSubterm ){
  
  for( var i=0 ; i<wayToSubterm.length ; i++ ){
    term = term[wayToSubterm[i]];
  }

  return term;
};

var allSubterms_byWays = function( term ){
  //TODO
};


/*

ttreePoses2WithTyps :: TTree -> ([(TTPos,Typ)],[(TTPos,Typ)])
ttreePoses2WithTyps t = 
  let xs  = poses2xx [] t 
      rev = map (\(pos,typ)->(reverse pos,typ))
   in ( rev . lefts $ xs , rev . rights $ xs )
 where
  poses2xx :: [Int] -> TTree -> [ Either ([Int],Typ) ([Int],Typ) ]
  poses2xx pos (TTree _ typ []) = [Left (pos,typ)]
  poses2xx pos (TTree _ typ ts) = 
   (Right (pos,typ)) : (concatMap (\(i,t)-> poses2xx (i:pos) t ) (zip [1..] ts) )


*/