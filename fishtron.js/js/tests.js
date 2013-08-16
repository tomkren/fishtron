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
      
      var funs = prove({
        n   : 3,
        typ : mkTyp(['a','a','a']),
        ctx : mkCtx({}) ,
        evalThem: true
      });

      strictEqual( 2 , funs.length  , '(a->a->a;{}) has 2 inhabitans' );
      deepEqual( [23,42] , _.map(funs,function(f){return f(23,42)}).sort() ,
          '... and those inhabitans are K and K*.' );

      var funs2 = prove({
         evalThem : true,
         n        : 100,
         typ      : mkTyp([int,int,int]),
         ctx      : mkCtx({ 
                      'p' : [ [int,int,int]
                            , function(x,y){return x+y;} ],
                      's' : [ [int,int]
                            , function(x){return x+1;} ] 
                    })
      });

      var supposedResult = [
        42, 23, 43, 24, 44, 25, 84, 65, 65, 46, 66, 47, 85, 66, 85, 66, 66, 47, 45, 
        26, 85, 66, 66, 47, 67, 48, 86, 67, 86, 67, 67, 48, 46, 27, 67, 48, 86, 67, 
        86, 67, 67, 48, 86, 67, 67, 48, 107, 88, 88, 69, 126, 107, 107, 88, 126, 107, 
        107, 88, 107, 88, 88, 69, 86, 67, 67, 48, 68, 49, 87, 68, 87, 68, 68, 49, 89, 
        70, 108, 89, 108, 89, 89, 70, 108, 89, 127, 108, 127, 108, 108, 89, 127, 108, 
        108, 89, 108, 89, 89, 70, 108, 89
      ];

      var realResult = _.map(funs2,function(f){return f(23,42);})

      deepEqual( supposedResult , realResult ,  
             'Check that implementation doesn\'t change on (Int->Int->Int;{+,s}) (100 terms). '+
             '(Fail doesn\'t necessary mean bug..)');


      console.timeEnd('tests');
    });

};
 
