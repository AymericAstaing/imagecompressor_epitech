module Utils where

import System.Environment
import System.Exit
import Data.List
import System.IO
import DataStructs
import System.Random

find_occurence str ch = [ y | (x, y) <- zip str [0..], x == ch ]

print_string str = print str
    
get_euclidian_distance (r1, g1, b1) (r2, g2 , b2) = sqrt (((r1 - r2) ^ 2) + ((g1 - g2) ^ 2) + ((b1 - b2) ^ 2))

get_color_euclidian_distance :: Color_Item -> (Double, Double, Double) -> Double
get_color_euclidian_distance item (r2, g2, b2) = sqrt ((((r item) - r2) ^ 2) + (((g item) - g2) ^ 2) + (((b item) - b2) ^ 2))

get_cluster_euclidian_distance :: Cluster -> (Double, Double, Double) -> Double
get_cluster_euclidian_distance item (r3, g3, b3) = sqrt ((((cl_r item) - r3) ^ 2) + (((cl_g item) - g3) ^ 2) + (((cl_b item) - b3) ^ 2))

get_cluster_color_euclidian_distance :: Cluster -> Color_Item -> Double
get_cluster_color_euclidian_distance cluster color = sqrt ((((cl_r cluster) - (r color)) ^ 2) + (((cl_g cluster) - (g color)) ^ 2) + (((cl_b cluster) - (b color)) ^ 2))

get_r_mean :: [Color_Item] -> Int -> Int -> Int
get_r_mean [] mean index = div mean index
get_r_mean (x:xs) mean index = get_r_mean xs (mean + (round (r x) :: Int)) (index + 1)

get_g_mean :: [Color_Item] -> Int -> Int -> Int
get_g_mean [] mean index = div mean index
get_g_mean (x:xs) mean index = get_g_mean xs (mean + (round (g x) :: Int)) (index + 1)

get_b_mean :: [Color_Item] -> Int -> Int -> Int
get_b_mean [] mean index = div mean index
get_b_mean (x:xs) mean index = get_b_mean xs (mean + (round (b x) :: Int)) (index + 1)

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

get_file_content :: [Char] -> IO String
get_file_content file_path = readFile file_path

appd_mean :: (Double, Double, Double) -> Color_Item -> (Double, Double, Double)
appd_mean (r_cl, g_cl, b_cl) color  = ((r_cl + (r color)), (g_cl + (g color)), (b_cl + (b color)))

toDouble :: String -> Double
toDouble str = read str :: Double

toInt :: String -> Int
toInt str = read str :: Int

no_modif :: Cluster -> Double -> Bool
no_modif c convergence = (get_euclidian_distance ((cl_r c), (cl_g c), (cl_b c))  ((prec_r c), (prec_g c), (prec_b c))) <= convergence

cluster_final_pos :: [Cluster] -> Double -> Bool
cluster_final_pos [] convergence = True
cluster_final_pos (x:xs) convergence = case (no_modif x convergence) of     True -> cluster_final_pos xs convergence
                                                                            _ -> False

str_to_word_tab :: String -> Char -> [String]
str_to_word_tab str delim = let (start, end) = break (== delim) str
    in start : if null end then [] else str_to_word_tab (tail end) delim

is_lower :: Double -> Color_Item -> Color_Item -> (Double, Double, Double) -> Color_Item -- distance actuelle 
is_lower distance_to_test tmp_lowest actual_item (r, g, b) =
    case ((get_color_euclidian_distance tmp_lowest (r, g, b)) > distance_to_test) of    True -> actual_item
                                                                                        _ -> tmp_lowest

is_lower_cluster :: Double -> Cluster -> Cluster -> (Double, Double, Double) -> Cluster -- distance actuelle 
is_lower_cluster distance_to_test tmp_lowest actual_item (r, g, b) =
    case ((get_cluster_euclidian_distance tmp_lowest (r, g, b)) > distance_to_test) of  True -> actual_item
                                                                                        _ -> tmp_lowest

-- usage de la récursivité
find_lowest_distance_into_list :: (Double, Double, Double) -> [Color_Item] -> Color_Item -> Color_Item
find_lowest_distance_into_list (r1, g1, b1) [] tmp_lower = tmp_lower
find_lowest_distance_into_list (r, g, b) (x:xs) tmp_lower_dist = (find_lowest_distance_into_list (r, g, b) xs (is_lower (get_color_euclidian_distance x (r, g, b)) tmp_lower_dist x (r, g, b)))

find_lowest_distance_into_cluster :: (Double, Double, Double) -> [Cluster] -> Cluster -> Cluster
find_lowest_distance_into_cluster (r1, g1, b1) [] tmp_lower = tmp_lower
find_lowest_distance_into_cluster (r, g, b) (x:xs) tmp_lower_dist = (find_lowest_distance_into_cluster (r, g, b) xs (is_lower_cluster (get_cluster_euclidian_distance x (r, g, b)) tmp_lower_dist x (r, g, b)))
