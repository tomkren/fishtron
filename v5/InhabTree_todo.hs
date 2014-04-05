module InhabTree 
( prove 
, SearchOptions(..)  
, kozaSearchOptions
, allEdgesSearchOptions
, geomSearchOptions  
) where

import TTerm (Typ,Context)  
--(Symbol,Typ(..),TTerm(..),Context,ttermTyp,toSki,toSki',checkTyp)




prove = undefined 


data SearchOptions = SearchOptions{
  so_n                  :: Int                , 
  so_typ                :: Typ                ,  
  so_ctx                :: Context            ,
  so_edgeSelectionModel :: EdgeSelectionModel ,
  so_randomRunState     :: RandomRunState     
 }

kozaSearchOptions     = undefined
allEdgesSearchOptions = undefined
geomSearchOptions     = undefined

data RandomRunState =
  NoRandomRunState |
  KozaRandomRunState (Maybe IsFullMethod) (Maybe MaximalDepth)  -- Nothing značí že chci aby se vygeneroval

type IsFullMethod = Bool
type MaximalDepth = Int

