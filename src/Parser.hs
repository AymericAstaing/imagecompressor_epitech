module Parser where

import Lib
import System.Exit
import System.IO
import Control.Monad
import System.Environment
import Utils
import Text.Printf
import DataStructs

parse_string :: [Char] -> Int -> Color_Item
parse_string str index = do
    let commas = find_occurence str ',' -- 2 9 12
    let left_par = find_occurence str '(' -- 0 6
    let right_par = find_occurence str ')' -- 4 16
    init_color_item (index, (toInt (substring 1 (commas !! 0) str)), (toInt (substring ((commas !! 0) + 1) (right_par !! 0) str)), (toDouble (substring ((left_par !! 1) + 1) (commas !! 1) str)), (toDouble (substring ((commas !! 1) + 1) (commas !! 2) str)), (toDouble (substring ((commas !! 2) + 1) (right_par !! 1) str)))

parse_pixel_file :: [String] -> Int -> [Color_Item]
parse_pixel_file [] index = []
parse_pixel_file (x:xs) index = (parse_string x index) : (parse_pixel_file xs (index + 1))