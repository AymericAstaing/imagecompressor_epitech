module Print where

import System.Environment
import System.Exit
import Data.List
import System.IO
import Text.Printf
import DataStructs
import System.Random


print_color_item :: Color_Item -> IO ()
print_color_item color = printf "(%d,%d) (%.f, %.f, %.f)\n"  (x color) (y color) (r color) (g color) (b color)

print_points :: Int -> [Color_Item] -> [(Int, Int)] -> IO ()
print_points cluster_id color_list [] = return ()
print_points cluster_id color_list (x:xs) = do
    if ((fst x) == cluster_id) then (print_color_item (color_list !! (snd x))) else return ()
    print_points cluster_id color_list xs

print_result :: [Cluster] -> [Color_Item] -> [(Int, Int)] -> IO ()
print_result [] color_list match_list = return ()
print_result (x:xs) color_list match_list = do
    printf "--\n(%.2f, %.2f, %.2f)\n-\n" (cl_r x) (cl_g x) (cl_b x)
    print_points (cl_id x) color_list match_list
    print_result xs color_list match_list