module Compressor where

import Lib
import System.Exit
import Prelude
import System.IO
import Control.Monad
import System.Environment
import System.Random
import Utils
import DataStructs

update_rgb :: Cluster -> (Double, Double, Double) -> Cluster
update_rgb c (r, g, b) = c  {
                                cl_r = r
                                , cl_g = g
                                , cl_b = b
                                , prec_r = (cl_r c)
                                , prec_g = (cl_g c)
                                , prec_b = (cl_b c)

                            }

random_cluster :: Int -> [Double] -> [Double] -> [Double] -> Int -> Cluster
random_cluster id rand_r rand_g rand_b rand_id = do
    init_cluster (id, (rand_r !! rand_id), (rand_g !! rand_id), (rand_b !! rand_id))

is_stop :: Int -> Int -> Bool
is_stop index max_cluster = ((index + 1) == max_cluster)
                              
initialise_cluster ::  [Double] -> [Double] -> [Double] -> Int -> Int ->  Bool -> [Cluster]
initialise_cluster rand_r rand_g rand_b max_cluster index True = []
initialise_cluster rand_r rand_g rand_b max_cluster index stop = (random_cluster index rand_r rand_g rand_b index) : (initialise_cluster rand_r rand_g rand_b max_cluster (index + 1) (is_stop index max_cluster))

find_nearest :: Color_Item -> [Cluster] -> Int
find_nearest item cluster_list = (cl_id (find_lowest_distance_into_cluster ((r item), (g item), (b item)) cluster_list (cluster_list !! 0)))

loop_throw_color_list :: [Color_Item] -> [Cluster] -> [(Int, Int)]
loop_throw_color_list [] cluster_list = []
loop_throw_color_list (x:xs) cluster_list = ((find_nearest x cluster_list), (co_id x)) : (loop_throw_color_list xs cluster_list)

append_pos :: (Double, Double, Double) -> (Int, Int) -> Int -> [Cluster] -> [Color_Item] -> (Double, Double, Double)
append_pos (r, g, b) match_item cluster_id clu_list col_list = 
    case ((fst match_item) == cluster_id) of    True -> appd_mean (r, g, b) (col_list !! (snd match_item))
                                                _ -> (r, g, b)

points_are_link :: Double -> Int -> [(Int, Int)] -> Double
points_are_link index cluster_id [] = index
points_are_link index cluster_id (x:xs) =
    case ((fst x) == cluster_id) of     True -> points_are_link (index + 1) cluster_id xs
                                        _ -> points_are_link index cluster_id xs

divide :: (Double, Double, Double) -> Int ->  [(Int, Int)] -> (Double, Double, Double)
divide (r, g, b) cluster_id match_list = do
    let match_nbr = points_are_link 0 cluster_id match_list
    (r / match_nbr, g / match_nbr, b / match_nbr)

get_new_pos :: [Cluster] -> [Color_Item] -> Int -> [(Int, Int)] -> (Double, Double, Double) -> (Double, Double, Double)
get_new_pos cluster_list color_list cluster_id [] (r, g, b) = (r, g, b) -- pos actuelle
get_new_pos cluster_list color_list cluster_id (x:xs) (r, g, b) = (get_new_pos cluster_list color_list cluster_id xs (append_pos (r, g, b) x cluster_id cluster_list color_list))

compute_mean :: Cluster -> [Cluster] -> [Color_Item] -> [(Int, Int)] -> Cluster
compute_mean  cluster_to_update cluster_list color_list linklist = update_rgb cluster_to_update (divide (get_new_pos cluster_list color_list (cl_id cluster_to_update) linklist (0, 0, 0)) (cl_id cluster_to_update) linklist)

update_clusters_means :: [Cluster] -> [Color_Item] -> [(Int, Int)] -> [Cluster]
update_clusters_means [] color_list links_list = []
update_clusters_means (x:xs) color_list  links_list = (compute_mean x xs color_list links_list) : (update_clusters_means xs color_list links_list)