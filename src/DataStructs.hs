module DataStructs where

import Lib

data Color_Item = Color_Item
    { co_id ::Int 
      ,x :: Int
      ,y :: Int
      ,r :: Double
      ,g :: Double
      ,b :: Double }
    
data Cluster = Cluster
    { cl_id :: Int
        ,prec_r :: Double
        ,prec_g :: Double
        ,prec_b :: Double
        ,cl_r :: Double
        ,cl_g :: Double
        ,cl_b :: Double }

init_color_item :: (Int, Int, Int, Double, Double, Double) -> Color_Item
init_color_item (id, x1, y1, r1, g1, b1) = Color_Item
        { 
        co_id = id
        , x = x1
        , y = y1
        , r = r1
        , g = g1
        , b = b1 }

init_empty_cluster :: Cluster
init_empty_cluster = Cluster
        { 
        cl_id = -1
        , prec_r = 0
        , prec_g = 0
        , prec_b = 0
        , cl_r = 0
        , cl_g = 0
        , cl_b = 0 }

init_cluster :: (Int, Double, Double, Double) -> Cluster
init_cluster (id, r, g, b) = Cluster
        { 
        cl_id = id
        , prec_r = 0
        , prec_g = 0
        , prec_b = 0
        , cl_r = r
        , cl_g = g
        , cl_b = b }

empty_color_item :: Color_Item
empty_color_item = Color_Item
        { 
        co_id = 0
        , x = 0
        , y = 0
        , r = 0
        , g = 0
        , b = 0 }
