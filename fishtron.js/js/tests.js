var fishtronTests = function(){

    
    test( 'PriorityQueue', function(){

      console.time('tests');

      var q = PriorityQueue();
      ok( q.isEmpty() , 'New queue is empty.' );
  
      q.push('a' , 100 ); // pushing 'a' with prority 100. The lower number, the higher priority. 
      ok( !q.isEmpty() , 'Queue with one element is not empty.'  );
        
      q.push('b',50)
       .push('c',75)
       .push('d',-50)
       .push('e',25)
       .push('f',0)
       .push('g',150);

      strictEqual( q.toString() , 'd (-50)\nf (0)\ne (25)\nb (50)\nc (75)\na (100)\ng (150)' , 
          'Method toString produces what is expected to.' );

      var arr1 = _.range(0,1000);
      var q2   = _.chain(arr1)
                  .shuffle()
                  .reduce(function(q,x){return q.push('',x)},PriorityQueue())
                  .value();
      var arr2 = [];
      while( !q2.isEmpty() ){
        arr2.push(q2.pop()[1]);
      }

      deepEqual( arr1, arr2, 'Shuffled array test (series of pushes and pops) : arrays are equal' );
      ok( q2.isEmpty() , '... and queue is empty.' )

    });
  
  
    test( 'prove()', function(){
      
      var res = prove({
        n   : 3,
        typ : mkTyp(['a','a','a']),
        ctx : mkCtx({}) ,
        resultMode: 'both'
      });

      strictEqual( 2 , res[1].length  , '(a->a->a;{}) has 2 inhabitans' );
      deepEqual( [23,42] , _.map(res[1],function(f){return f(23,42)}).sort() ,
          '... and those inhabitans are K and K*.' );
      ok( areTermsNondecreasing(res[0]) , '... and their size is nondecreasing.' );

      var int = mkAtm('int');

      res = prove({
         resultMode: 'both',
         n        : 100,
         typ      : mkTyp([int,int,int]),
         ctx      : mkCtx({ 
                      'p' : [ [int,int,int]
                            , function(x,y){return x+y;} ],
                      's' : [ [int,int]
                            , function(x){return x+1;} ] 
                    })
      });
      
      var realResult = _.map(res[1],function(f){return f(23,42);})

      var supposedResult = [
        42, 23, 43, 24, 44, 25, 84, 65, 65, 46, 66, 47, 85, 66, 85, 66, 66, 47, 45, 
        26, 85, 66, 66, 47, 67, 48, 86, 67, 86, 67, 67, 48, 46, 27, 67, 48, 86, 67, 
        86, 67, 67, 48, 86, 67, 67, 48, 107, 88, 88, 69, 126, 107, 107, 88, 126, 107, 
        107, 88, 107, 88, 88, 69, 86, 67, 67, 48, 68, 49, 87, 68, 87, 68, 68, 49, 89, 
        70, 108, 89, 108, 89, 89, 70, 108, 89, 127, 108, 127, 108, 108, 89, 127, 108, 
        108, 89, 108, 89, 89, 70, 108, 89
      ];

      
      deepEqual( supposedResult , realResult ,  
             'Check that implementation doesn\'t change on (Int->Int->Int;{+,s}) (100 terms). '+
             '(Fail doesn\'t necessary mean bug..)');

      ok( areTermsNondecreasing(res[0]) , '... and again their size is nondecreasing.' );




      res = prove({
         resultMode : 'both',
         n        : 100,
         typ      : mkTyp(['int','int','int']),
         ctx      : mkCtx({
                      ap42 : [ [['int','int'],'int'],
                             function(f){return f(42)}  ],
                      p :    [ ['int','int','int'],
                             function(x,y){return x+y;} ],
                      s :    [ ['int','int'],
                             function(x){return x+1;} ] 
                    })
      });

      realResult = _.map(res[1],function(f){return f(23,42);});

      supposedResult = [
        42, 23, 43, 24, 44, 25, 84, 65, 65, 46, 42, 42, 23, 43, 43, 24, 66, 47, 85, 66, 85, 66, 
        66, 47, 45, 26, 85, 66, 66, 47, 43, 43, 24, 44, 44, 25, 67, 48, 86, 67, 86, 67, 67, 48, 
        46, 27, 67, 48, 86, 67, 86, 67, 67, 48, 86, 67, 67, 48, 44, 44, 25, 84, 84, 65, 84, 84, 
        65, 65, 65, 46, 42, 42, 42, 23, 107, 88, 88, 69, 65, 65, 46, 126, 107, 107, 88, 84, 84, 
        65, 126, 107, 107, 88, 107, 88, 88, 69, 84, 65, 84, 65
      ];
      
      deepEqual( supposedResult , realResult , 'Similar check on the slightly bigger ctx.');

      ok( areTermsNondecreasing(res[0]) , '... size is nondecreasing.' );

    });
  
  
    test( 'xover', function(){
      
      var terms = prove({
         n        : 100,
         typ      : mkTyp(['int','int','int']),
         ctx      : mkCtx({
                      ap42 : [ [['int','int'],'int'],
                             function(f){return f(42)}  ],
                      p :    [ ['int','int','int'],
                             function(x,y){return x+y;} ],
                      s :    [ ['int','int'],
                             function(x){return x+1;} ] 
                    })
      });
      

      
      ok( _.every( terms, function(t){return allWays_mustBeTrue(t,'atTree');} ) , 
         'Number of ways (positions of subterms) in term is eq to number of term nodes '+
         '(tested on 100 generated terms) .... @-tree variant.' );
      
      ok( _.every( terms, function(t){return allSubterms_mustBeTrue(t,'atTree');} ) , 
         'Actually generates subterms and count them, to test that their number'+
         ' is the same as the number of term nodes ... again @-tree.' );

      ok( _.every( terms, function(t){return allWays_mustBeTrue(t,'sexprTree');} ) , 
         '... test 1 with sexprTree.' );
      
      ok( _.every( terms, function(t){return allSubterms_mustBeTrue(t,'sexprTree');} ) , 
         '... test 2 with sexprTree.' );
         
      ok( _.every(terms,invariant_partitionedByIsLeaf) , 'Testing invariant_partitionedByIsLeaf.' );


      var tt1 = {"c":"lam","x":"_0","m":{"c":"lam","x":"_1","m":{"c":"app","m":{"c":"app","m":{"c":"val",
      "x":"p","t":{"c":"arr","a":{"c":"atm","a":"int"},"b":{"c":"arr","a":{"c":"atm","a":"int"},
      "b":{"c":"atm","a":"int"}}}},"n":{"c":"app","m":{"c":"val","x":"s","t":{"c":"arr",
      "a":{"c":"atm","a":"int"},"b":{"c":"atm","a":"int"}}},"n":{"c":"var","x":"_0",
      "t":{"c":"atm","a":"int"}},"t":{"c":"atm","a":"int"}},"t":{"c":"arr","a":{"c":"atm","a":"int"},
      "b":{"c":"atm","a":"int"}}},"n":{"c":"var","x":"_0","t":{"c":"atm","a":"int"}},
      "t":{"c":"atm","a":"int"}},"t":{"c":"arr","a":{"c":"atm","a":"int"},"b":{"c":"atm","a":"int"}}},
      "t":{"c":"arr","a":{"c":"atm","a":"int"},"b":{"c":"arr","a":{"c":"atm","a":"int"},
      "b":{"c":"atm","a":"int"}}}};
      
      var tt2 = {"c":"lam","x":"_0","m":{"c":"lam","x":"_1","m":{"c":"app","m":{"c":"app","m":
      {"c":"val","x":"p","t":{"c":"arr","a":{"c":"atm","a":"int"},"b":{"c":"arr","a":{"c":"atm",
      "a":"int"},"b":{"c":"atm","a":"int"}}}},"n":{"c":"app","m":{"c":"val","x":"ap42","t":
      {"c":"arr","a":{"c":"arr","a":{"c":"atm","a":"int"},"b":{"c":"atm","a":"int"}},"b":
      {"c":"atm","a":"int"}}},"n":{"c":"lam","x":"_2","m":{"c":"var","x":"_1","t":{"c":"atm","a":"int"}},
      "t":{"c":"arr","a":{"c":"atm","a":"int"},"b":{"c":"atm","a":"int"}}},"t":{"c":"atm","a":"int"}},
      "t":{"c":"arr","a":{"c":"atm","a":"int"},"b":{"c":"atm","a":"int"}}},"n":{"c":"var","x":"_0",
      "t":{"c":"atm","a":"int"}},"t":{"c":"atm","a":"int"}},"t":{"c":"arr","a":{"c":"atm","a":"int"},
      "b":{"c":"atm","a":"int"}}},"t":{"c":"arr","a":{"c":"atm","a":"int"},"b":{"c":"arr",
      "a":{"c":"atm","a":"int"},"b":{"c":"atm","a":"int"}}}};
    
      var way  = allWays(tt1)[8];
      var newS = allSubterms_byWays(tt2)[5];

      strictEqual( code(changeSubterm(tt1,way,newS).newTerm) ,  
                   'function(_0,_1){return p(s(_0),ap42(function(_2){return _1}))}' ,
                   'Primitive changeSubterm test on two arbitrary terms (swapping their subterms).' );      


      console.timeEnd('tests');
    });

};



var invariant_partitionedByIsLeaf = function( term ){
  var ways  = allWays(term,'atTree');
  var pWays = partition( isWayToLeaf , ways );

  var ok1 = _.every( pWays.satisfy , function(way){
    var sTerm = subterm(term,way);
    return (sTerm.t === way.t) && ( isVar(sTerm) || isVal(sTerm) );
  });

  var ok2 = _.every( pWays.notSatisfy , function(way){
    var sTerm = subterm(term,way);
    return (sTerm.t === way.t) && ( isApp(sTerm) || isLam(sTerm) );
  });

  return ok1 && ok2 ;
};



var allWays_mustBeTrue = function( term , mode ){
  return allWays(term,mode).length === termSize(term,{countAPPs:(mode!=='sexprTree')});
};
          
var allSubterms_byWays = function( term , mode ){
  var ways = allWays(term,mode);
  return _.map(ways,function(way){return subterm(term,way);});
};
       
var allSubterms_mustBeTrue = function( term , mode ){
  return allSubterms_byWays(term,mode).length === termSize(term,{countAPPs:(mode!=='sexprTree')});
};


var isNondecreasing = function(xs){
  for(var i = 0 ; i < xs.length-1 ; i++ ){
    if( xs[i] > xs[i+1] ){
      return false;
    }
  }
  return true;
};
  
var areTermsNondecreasing = function(terms){
  return isNondecreasing(_.map(terms, function(term){
    return termSize(term, {countAPPs:false} );
  }));
};
 
