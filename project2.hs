-- Author  : Hanyi Gao <hanyig1@student.unimelb.edu.au>
-- ID      : 1236476
-- Project : proj2, COMP90048
-- Purpose : Implement the guessing and answering parts of a logical guess game.


-- | This file implements some parts of a guess game in which there is a hider
-- and a guesser. The hider hides 3 ships in 3 different locations on a 4×8
-- grid while the gueeser needs to guess the exact locations of the ships. The
-- guesser keeps guessing based on the feedback of the previous guess. The 
-- program needs to implement both the guessing part and the feedback part. And
-- the guesser in the program needs to find all the hidden ships as fewer number
-- of guesses as possible. To achieve that goal, the number of possible targets
-- will be reduced based on the feedback from previous guess and the next guess
-- which has the highest possibility to get the answer will be made after some
-- calculations.


module Proj2 (Location, toLocation, fromLocation, feedback,
             GameState, initialGuess, nextGuess) where


import Data.List
import Data.Char (ord)
import Data.Map (fromListWith, toList)


-- | some pre-defined types
type Feedback = (Int, Int, Int)
type TargetFb = ([Location], Feedback)
-- | Store remaining targets and their expected feedbacks(based on the guess).
type GameState = [TargetFb]


-- | Location type is defined here. The first Char type indicates which column
-- the location is in while the second char type indicates which row the location
-- is in. e.g. Location 'A' '1' just simply represents A1.
data Location = Location Char Char
    deriving (Show, Eq)


-- | The initial guess is defined here.
-- We can actually get the initial guess based on the takeGuess function 
-- written below. However, to reduce the time of calculaion, I just put the 
-- result directly in this program.
initLocs :: [Location]
initLocs = [(Location 'A' '1'), (Location 'H' '1'), (Location 'H' '4')]


-- | Get all the subsets of size equal to a certain value (the first argument) 
-- from a set(the second argument representing as a list)
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs


-- | Take the column of Char type as input and check if the column information 
-- given is valid.
isColValid :: Char -> Bool
isColValid c = c >= 'A' && c <= 'H'


-- | Take the row of Char type as input and Check if the row information given 
-- is valid.
isRowValid :: Char -> Bool
isRowValid r = r >= '1' && r <= '4'


-- | Check if one ship(the second argument) is away from the other ship
-- (the third argument) in a given range(the first argument).
inRange :: Int -> Location -> Location -> Bool
inRange dist (Location tc tr) (Location gc gr) =
    abs (ord tc - ord gc) <= dist && abs (ord tr - ord gr) <= dist 


-- | Check if one of the guesses is away from any of the target ships in a given 
-- range.
inRangeOfTargets :: Int -> [Location] -> Location -> Bool
inRangeOfTargets _ [] _ = False
inRangeOfTargets dist (x:xs) guessLoc
    | inRange dist x guessLoc = True
    | otherwise = inRangeOfTargets dist xs guessLoc


-- | Get the feedback of a ship in a target.
oneShipFeedback :: Int -> [Location] -> Location -> ([Int], Bool)
oneShipFeedback 0 targetLocs guessLoc
    | elem guessLoc targetLocs = ([1], True)
    | otherwise = ([0], False)
oneShipFeedback dist targetLocs guessLoc = 
    if hasCloserLoc 
    then (res ++ [0], True)
    else
        (
         if (inRangeOfTargets dist targetLocs guessLoc) 
         then (res ++ [1], True)
         else (res ++ [0], False)
        )
    where (res, hasCloserLoc) = oneShipFeedback (dist - 1) targetLocs guessLoc


-- | Get all the locations.
allLoc :: [Location]
allLoc = [(Location col row) | col <- cols, row <- rows] where
    cols = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
    rows = ['1', '2', '3', '4']


-- | Given a target(first argument) and a guess(second argument), add feedback 
-- to the target and return a turple of the target and its feedback
addFbToTarget :: [Location] -> [Location] -> TargetFb
addFbToTarget target guess = (target, (feedback target guess)) 


-- | Take all the targets from the GameState.
-- | The game state includes all the remaining targets and their expected 
-- feedbacks, and we may only need targets and do not care about feedback 
-- information sometimes.
takeTargets :: GameState -> [[Location]]
takeTargets gameState = [target | (target, _) <- gameState]


-- | Remove the inconsistent targets in the GameState based on the feedback from 
-- the last guess. Take the previous game state and the feedback as inputs and
-- return the new game state.
removeInconsistentEle :: GameState -> Feedback -> GameState
removeInconsistentEle gameState feedback = 
    filter (\(_, y) -> y == feedback) gameState


-- | For one possible guess(the first argument), get the feedback set from all 
-- possible targets(the second argument).
feedbackSet :: [Location] -> [[Location]] -> [TargetFb]
feedbackSet guess allTargets = 
    [addFbToTarget possibleTarget guess | possibleTarget <- allTargets]


-- | Get the numbers of occurrences of different target's feedbacks.
frequency :: [TargetFb] -> [(Feedback, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | (_, x) <- xs])


-- | Get the numebr of expected remaining candidates of one possible guessing 
-- target based on the numbers of occurrences of target's feedback(the second 
-- argument) and the total number of feedback set(the first argument).
expectedNum :: Int -> [(Feedback, Int)] -> Float
expectedNum total xs = (fromIntegral v) / (fromIntegral total)
    where v = sum (map (^2) [x | (_, x) <- xs])


-- | Get the number of expected remaining candidates of one possible guessing 
-- target (the first argument) based on the remaining targets(the second argument). 
mapExpectedNum :: [Location] -> [[Location]] -> ([Location], Float)
mapExpectedNum target allTargets =
    (target, expectedNum (length allTargets) freqList) where
    freqList = frequency (feedbackSet target allTargets)


-- | Get the next guessing target based on the current remaining candidates.
takeGuess :: [[Location]] -> [Location]
takeGuess allTargets = 
    minValueTarget (map (\target -> mapExpectedNum target allTargets) allTargets) 
    
    
-- | Get the target with minimum expected number from all the remaining 
-- candidates as the next guess.
minValueTarget :: [([Location], Float)] -> [Location]
minValueTarget [(target, _)] = target
minValueTarget ((targetX, valX):(targetY, valY):xs)
    | valX > valY = minValueTarget ((targetY, valY):xs)
    | otherwise = minValueTarget ((targetX, valX):xs)
   

-- ------------------------------------------------------------
-- Below are the functions that must be defined in this project.

-- | Gives Just the Location named by the string, or Nothing if the string 
-- is not a valid location name.
toLocation :: String -> Maybe Location
toLocation [c, r] 
    | isColValid c && isRowValid r = Just (Location c r)
    | otherwise = Nothing
toLocation _ = Nothing


-- | Get a two-character string version of a location.
fromLocation :: Location -> String
fromLocation (Location c r) = [c] ++ [r]


-- | Take a target(the first argument) and a guess(the second argument), and 
-- returns the feedback of the guess.
feedback :: [Location] -> [Location] -> Feedback
feedback _ [] = (0, 0, 0)
feedback targetLocs (x:xs) = (a + a', b + b', c + c') where 
    ([a, b, c], _) = oneShipFeedback 2 targetLocs x
    ((a', b', c')) = feedback targetLocs xs


-- | Get a pair of an initial guess and a game state.
initialGuess :: ([Location], GameState)
initialGuess = (initLocs, (map (\x -> addFbToTarget x initLocs) (subsets 3 allLoc)))


-- | Take as input a pair of the previous guess and game state, and the feedback 
-- to this guess and returns a pair of the next guess and new game state.
nextGuess :: ([Location],GameState) -> Feedback -> ([Location],GameState)
nextGuess (_, gs) feedback = 
    (guess, feedbackSet guess candidates) where
    guess = takeGuess candidates
    candidates = takeTargets (removeInconsistentEle gs feedback) 
        




