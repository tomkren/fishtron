module Music 
(
) where



import Euterpea

i3 = SlapBass2

song :: Music Pitch
song = let i1  = i3 --Vibraphone
           i2  = Gunshot --HonkyTonkPiano --Ocarina --ynthBass1
           t1  = (instrument i1 pat2   :=: instrument i2 pat2   ) 
           t2  = (instrument i1 pat2'                           ) 
           t2' = (instrument i1 pat2'' :=: instrument i2 pat2'' ) 
           t3  = (instruments [i2,i1] pat3   )
        in t1 :+: t2 :+: t2' :+: t3

instruments is pat = foldr (\ x acc -> ( instrument x pat :=: acc ) ) (rest 0) is

pat2  = pat1 :+: pat1' 
pat2' = pat1' :+: pat1 
pat2''= pat1'':+: pat1 

pat3  = c' 4 0.1 :+: c' 4 0.1 :+: c' 5 0.05 :+: c' 5 0.1

pat1  = c' 4 0.1 :+: c' 4 0.1 :+: c' 5 0.05 :+: c' 4 0.1
pat1' = c' 5 0.1 :+: c' 4 0.1 :+: c' 3 0.05 :+: c' 4 0.1
pat1''= c' 4 0.1 :+: c' 4 0.1 :+: c' 4 0.05 :+: c' 4 0.1

n c x dur = (note dur (c,x))

c' :: Octave -> Dur -> Music Pitch 
c' x dur = (note dur (C,x)) :+: rest ( dur * 0.5) 

c4 :: Dur -> Music Pitch
c4 dur = note dur (C,4::Octave)

pat0 i  =foldr1 (:+:) $ take 42 $ pa0 (-3) 0.999
pa0 i q = (c' (i+4) (0.1*q)) : (c' (i+4) (0.1*q)) : (c' (i+5) (0.05*q)) : (c' (i+4) (0.1*q)) : pa0 (i+1) (q*q)

pat0' i  =foldl1 (:+:) $ take 42 $ pa0' (5) 0.999
pa0' i q = (c' (i+4) (0.1*q) ) : (c' (i+4) (0.1*q) ) : (c' (i+3) (0.05*q) ) : (c' (i+4) (0.1*q) ) : pa0' (i-1) (q*q)

song2 = pat0 0 :=: (instrument i3 $ pat0' (-5))

wts :: Pitch -> [Music Pitch ]
wts p = let f ap = note qn (pitch (absPitch p + ap))
         in map f [0, 2, 4, 6, 8]

