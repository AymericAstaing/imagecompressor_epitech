module Main where

import Lib
import System.Exit
import System.Random
import System.IO
import Control.Monad
import System.Environment
import Utils
import Parser
import Text.Printf
import DataStructs
import Compressor
import Print

argument_check argv = ((length argv) == 3) || (((length argv) == 1) && ((argv !! 0) == "-h"))

anormal_parameters :: IO ()
anormal_parameters = do
    usage
    (exitWith (ExitFailure 84))

help_required argv = (((length argv) == 1) && (argv !! 0) == "-h")

usage :: IO()
usage = do
    x <- readFile "help"
    putStr x

print_help :: IO()
print_help = do
    usage
    exitSuccess

check_exists :: (FilePath -> IO Bool) -> FilePath -> IO ()
check_exists p s = do
    res <- p s
    if res
        then return ()
        else (exitWith (ExitFailure 84))

loop :: [Color_Item] -> [Cluster] -> Double -> IO ()
loop color_list cluster_list convergence = do
    let match_list = loop_throw_color_list color_list cluster_list
    let new = update_clusters_means cluster_list color_list match_list
    case ((cluster_final_pos new convergence) == True) of   True -> (print_result new color_list match_list)
                                                            _ -> loop color_list new convergence


spli :: Eq a => a -> [a] -> [[a]]
spli x y = func x y [[]]
    where
        func x [] z = reverse $ map (reverse) z
        func x (y:ys) (z:zs) = if y == x then 
            func x ys ([]:(z:zs)) 
        else 
            func x ys ((y:z):zs)

main :: IO ()
main = do
    args <- getArgs
    when ((argument_check args) == False) (anormal_parameters)
    when ((help_required args) == True) (print_help)
    s <- (get_file_content (args !! 2))
    let pixel_list = spli '\n' s
    let final = if ((length (pixel_list !! ((length pixel_list) - 1))) == 0) then (init pixel_list) else (pixel_list)
    let color_list = parse_pixel_file final 0
    let mean_r = (fromIntegral (get_r_mean color_list 0 0)) :: Double
    let mean_g = (fromIntegral (get_g_mean color_list 0 0)) :: Double
    let mean_b = (fromIntegral (get_b_mean color_list 0 0)) :: Double
    let max_r = if ((mean_r + 10) > 255) then (255) else (mean_r + 10)
    let min_r = if ((mean_r - 10) < 0) then (0) else (mean_r - 10)
    let max_g = if ((mean_g + 10) > 255) then (255) else (mean_g + 10)
    let min_g = if ((mean_g - 10) < 0) then (0) else (mean_g - 10)
    let max_b = if ((mean_b + 10) > 255) then (255) else (mean_b + 10)
    let min_b = if ((mean_b - 10) < 0) then (0) else (mean_b - 10)
    random_cl_r <- sequence $ replicate (toInt (args !! 0)) $ randomRIO (min_r,max_r::Double)
    random_cl_g <- sequence $ replicate (toInt (args !! 0)) $ randomRIO (min_g,max_g::Double)
    random_cl_b <- sequence $ replicate (toInt (args !! 0)) $ randomRIO (min_b,max_b::Double)
    let clusters = initialise_cluster random_cl_r random_cl_g random_cl_b (toInt(args !! 0)) 0 False
    loop color_list clusters (toDouble (args !! 1))