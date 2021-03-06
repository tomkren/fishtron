module Problems.Fly02.Funs_old where


type Input_  = ( [Pos] , Maybe Pos , Pos , Energy )
type Output_ = Dir_
type Dir_    = Int
type Energy  = Int
type Pos = (Int,Int)




s :: (a->b->c) -> (a->b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x

if' :: Bool -> a -> a -> a
if' p q r = if p then q else r 

head_ :: [a] -> Maybe a
head_ xs = case xs of
  []  -> Nothing
  x:_ -> Just x

dStay , dUp , dDown , dLeft , dRight :: Dir_
dStay   = 0 
dUp     = 1
dDown   = 2
dLeft   = 3 
dRight  = 4 

output_ :: Dir_ -> Output_
output_ = id

myApplePoses_ :: Input_ -> [Pos]
myApplePoses_ (x,_,_,_) = x

nearestFlyPos_ :: Input_ -> Maybe Pos
nearestFlyPos_ (_,x,_,_) = x

myPos_ :: Input_ -> Pos
myPos_ (_,_,x,_) = x

inputEnergy_ :: Input_ -> Energy
inputEnergy_ (_,_,_,x) = x

posToDir_ :: Pos -> Maybe Pos -> Dir_
posToDir_ posMy Nothing = dStay 
posToDir_ posMy (Just posHer) 
  |   dx  > dy && (-dx) > dy = dUp
  |   dx  > dy               = dRight
  | (-dx) > dy               = dLeft
  | otherwise                = dDown
 where (dx,dy) = posHer `minus` posMy

avg :: [Pos] -> Maybe Pos
avg xs = case xs of
  [] -> Nothing
  xs -> let (xs1,xs2) = unzip xs
         in Just ( round $ mean xs1 , round $ mean xs2 )
 where
  mean :: [Int] -> Double
  mean xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)



minus :: Pos -> Pos -> Pos
minus (x1,y1) (x2,y2) = (x1-x2,y1-y2) 

dist :: Pos -> Pos -> Double
dist (x1,y1) (x2,y2) = sqrt $ (d2 x1 x2) + (d2 y1 y2)
 where d2 a b = let c=a-b in fromIntegral $ c*c 
