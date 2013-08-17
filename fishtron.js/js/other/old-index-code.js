$(function(){

  int  = mkAtm('int');
  char = mkAtm('char');

  x = mkVar('x',int);


  s = mkVal('s', mkTyp([int,int]) );

  f = mkLam(x, mkApp(s,x)  );

  one = mkVal(1,int);

  plus = mkVal( 'plus' , mkTyp([int,int,int])  );

  m = mkApp( f , one );

  //log( 'm = ' + code(m,'lc') );
  //log( 'm = ' + code(m,'jsstr') );


  g = mkVal( function(x){return x+1;} , mkArr(int,int) );

  ctx_ = {
   'plus' : { t : mkArr(int,mkArr(int,int))  
            , f : function(x){return function(y){return x+y;}; } },
   's'    : { t : mkArr(int,int)
            , f : function(x){return x+1;} }
  };

  ctx = mkCtx({
    'p' : [ [int,int,int]
          , function(x,y){return x+y;} ],
    's' : [ [int,int]
          , function(x){return x+1;} ]
  });


  ctx2 = mkCtx({
    'omg'  : [ [int,[int,int,int],[char,char],int]
             , function(x,y,z){return x;}  ],
    'plus' : [ [int,int,int]
           , function(x){return function(y){return x+y;}; }  ],
    's'    : [ [int,int]
           , function(x){return x+1;} ]
  });


  if( false ){

    console.time('term-generation');
    terms = prove({
      n     : 100,
      typ   : mkTyp([int,int,int]),
      ctx   : ctx ,
      logit : true
    });
    console.timeEnd('term-generation');
  
  
    console.time('term-evaluation');
    funs = evalTerms(terms,ctx);
    log(_.map(funs,function(f){return f(23,42);}));
    console.timeEnd('term-evaluation');

  }

  // console.time('term-evaluation-2');
  // funs = evalTerms_2(terms,ctx);
  // log(_.map(funs,function(f){return f(1,2);}));
  // console.timeEnd('term-evaluation-2');
           
  // console.time('term-evaluation-slow');
  // funs = evalTerms_slow(terms,ctx);
  // log(_.map(funs,function(f){return f(1,2);}));
  // console.timeEnd('term-evaluation-slow');


/*
  s0 = mkStartZipper(mkTyp([int,char,int]),ctx);
  log(showZipper(s0));
  s1 = expandLam(s0);
  log(showZipper(s1.zipper) + ' >>> dist=' + s1.dist );
  s2 = gotoNextUnf(s1.zipper);
  log(showZipper(s2));
  s3 = expandApp(s2);
  s4 = [];
  s5 = [];
  _.each(s3,function(z,i){
    s4[i] = z ;
    s5[i] = gotoNextUnf(z) ;
    log('  ' + showZipper(z));
    log('    ' + showZipper(s5[i]));
  });

  log('---------------------------');

  var logz = function(z){return log(showZipper(z))};

  z0 = mkStartZipperSmart( mkTyp([int,char,int]),ctx2 );
  logz( z0 );
  zs1 = smartExpand(z0);
  _.each(zs1,function(z,i){
    //z2[i] = z.zipper ;
    log('  ' + showZipper(z.state) + '  >>> dist=' + z.dist );
  });


  log('------------------------------');
*/

  
});