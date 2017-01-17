module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList =
    WordList [String]
    deriving (Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w =
              let l = length (w :: String)
              in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
    show (Puzzle _ discovered guessed badGuesses) =
        intersperse ' ' (fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed
        ++ " Number of bad guesses: " ++ show badGuesses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just x) = x
renderPuzzleChar Nothing = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle w =
    let discovered = map (const Nothing) w
        in Puzzle w discovered [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = c `elem` g

fillInCharacter :: Puzzle -> Char -> Int -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s _) c badGuesses =
    Puzzle word newFilledInSoFar (c : s) badGuesses
    where zipper guessed wordChar guessChar =
              if wordChar == guessed
                 then Just wordChar
                 else guessChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle _ _ _ n) guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
         (_, True) -> do
            putStrLn "You already guessed that\
                 \ character, pick something else!"
            return puzzle
         (True, _) -> do
             putStrLn "This character was in the word,\
                 \ filling in the word accordingly"
             return (fillInCharacter puzzle guess n)
         (False, _) -> do
             putStrLn "This character wasn't in\
                 \ the word, try again."
             return (fillInCharacter puzzle guess (n+1))

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed badGuesses) =
    if badGuesses > 7 then
                      do putStrLn "You lose!"
                         putStrLn $ "The word was: " ++ wordToGuess
                         exitSuccess
    else return ()

gameWin :: Puzzle -> IO()
gameWin (Puzzle _ filledInSoFar _ _) =
    if all isJust filledInSoFar then
                                do putStrLn "You win!"
                                   exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
         [c] -> handleGuess puzzle c >>= runGame
         _   -> putStrLn "Your guess must\
                            \ be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

