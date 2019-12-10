-- Mark Judy
-- My advent of code 2019 haskell solutions

module Main where

import Data.List

-- Main function for testing compiled performance ------------------------------

main = do putStrLn $ show $ day1p1 [1..100000000]

-- Helper functions ------------------------------------------------------------

-- if' :: Bool -> a -> a -> a
-- if' True x _ = x
-- if' False _ y = y

replace :: Int -> a -> [a] -> [a]
replace i e xs = case splitAt i xs of
    (before, _:after) -> before ++ e: after
    _ -> xs

-- Day 1 -----------------------------------------------------------------------

modules :: [Int]
modules = 
    [ 74099,  50130,  81867,  55356,  73088,  73706,  55902,  113399, 129578, 78051
    , 117663, 137454, 66285,  115389, 50547,  51588,  115792, 91085,  118882, 109486
    , 135616, 107771, 90105,  101182, 54766,  86615,  91261,  104321, 121607, 82197
    , 68626,  111255, 136080, 87509,  70125,  91180,  75925,  53492,  96853,  115081
    , 121621, 87461,  116030, 67335,  61282,  112424, 106785, 142243, 110564, 56983
    , 131420, 116534, 117376, 147088, 117628, 53964,  73163,  106736, 76217,  128590
    , 116138, 66841,  109265, 106285, 64013,  78357,  125640, 145761, 139426, 127558
    , 135076, 130989, 68054,  134669, 144482, 125870, 112818, 60193,  107162, 112557
    , 115972, 50890,  148652, 89547,  120228, 85967,  103941, 130915, 129496, 66401
    , 87018,  149539, 105847, 60981,  82610,  134396, 121711, 142655, 104400, 103752
    ]

-- Sums how much fuel is needed for a list of modules
day1p1 :: [Int] -> Int
day1p1 = sum . fmap fuelformass1

-- Calculates how much fuel a given mass requires
fuelformass1 :: Int -> Int
fuelformass1 m = m `quot` 3 - 2

-- Sums how much fuel is needed for each module and its fuel
day1p2 :: [Int] -> Int
day1p2 = sum . fmap fuelformass2

fuelformass2 :: Int -> Int
fuelformass2 m = sum $ takeWhile (> 0) $ iterate fuelformass1 (fuelformass1 m)

-- Day 2 -----------------------------------------------------------------------

instructions :: [Int]
instructions =
    [ 1,   0,    0,   3
    , 1,   1,    2,   3
    , 1,   3,    4,   3
    , 1,   5,    0,   3
    , 2,  10,    1,  19
    , 1,  19,    5,  23
    , 1,  23,    9,  27
    , 2,  27,    6,  31
    , 1,  31,    6,  35
    , 2,  35,    9,  39
    , 1,   6,   39,  43
    , 2,  10,   43,  47
    , 1,  47,    9,  51
    , 1,  51,    6,  55
    , 1,  55,    6,  59
    , 2,  59,   10,  63
    , 1,   6,   63,  67
    , 2,   6,   67,  71
    , 1,  71,    5,  75
    , 2,  13,   75,  79
    , 1,  10,   79,  83
    , 1,   5,   83,  87
    , 2,  87,   10,  91
    , 1,   5,   91,  95
    , 2,  95,    6,  99
    , 1,  99,    6, 103
    , 2, 103,    6, 107
    , 2, 107,    9, 111
    , 1, 111,    5, 115
    , 1, 115,    6, 119
    , 2,   6,  119, 123
    , 1,   5,  123, 127
    , 1, 127,   13, 131
    , 1,   2,  131, 135
    , 1, 135,   10,   0
    , 99 
    , 2,  14,    0,   0
    ]

-- "Runs" a list of instructions. The instructions are:
--      1, a, b, c (add pos1 pos2 destination)
--      2, a, b, c (mul pos1 pos2 destination)
--      99         (halt)
-- The postions and destinations refer to postions in the program itself.
-- Reads first opcode, preforms associated action,then runs the next 
-- instruction, until the halt instruction is encountered.
-- Returns the resulting list of instructions.
runprogram :: Int -> [Int] -> [Int]
runprogram pos program = case program !! pos of
    1 -> runprogram (pos + 4) $ replace dest (val1 + val2) program
    2 -> runprogram (pos + 4) $ replace dest (val1 * val2) program
    99 -> program
    ii -> error $ "Invalid instruction " ++ (show ii) ++ " at " ++ (show pos)
    where
        val1 = program !! (program !! (pos + 1))
        val2 = program !! (program !! (pos + 2))
        dest = program !! (pos + 3)

-- Restores the gravity assist program and then returns the value at position 0.
-- Replaces position 1 with the value 12, postion 2 with the value 2,
-- then runs the program and returns the value at position 0.
day2p1 :: [Int] -> Int
day2p1 program = (runprogram 0 $ replace 2 2 $ replace 1 12 program) !! 0

-- Finds values to replace position 1 and 2 with so that the program outputs 19690720
-- Noun and verb will be from 0 to 99 inclusive
day2p2 :: Int -> Int -> [Int] -> (Int, Int)
day2p2 noun verb program = 
    case (runprogram 0 $ replace 2 verb $ replace 1 noun program) !! 0 of
        19690720 -> (noun, verb)
        _ -> case (noun > 99, verb > 99) of
            (False, True) -> day2p2 (noun + 1) 0 program
            (True, True) -> error "Noun and verb exceeded 99"
            _ -> day2p2 noun (verb + 1) program

-- Day 3 -----------------------------------------------------------------------

