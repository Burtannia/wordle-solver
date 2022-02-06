{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Arrow    ()
import           Data.Bifunctor   (bimap)
import           Data.Foldable    (foldl', maximumBy, minimumBy)
import           Data.List        (all, any, filter, init, last, length, lines,
                                   map, sortBy, unlines, zipWith, (!!))
import           Prelude          hiding (readFile)
import           System.IO.Strict (readFile)
import           System.Random    (mkStdGen, randomR)

fiveLetters :: String -> Bool
fiveLetters = (==) 5 . length

prepDictionary :: IO ()
prepDictionary = do
    ws <- filter fiveLetters . map init . lines <$> readFile "enable1.txt"
    writeFile "wordle.txt" $ unlines ws

data Colour = Grey | Yellow | Green
    deriving (Eq, Show, Read)

data Square = Sq Char Colour
    deriving (Eq, Show, Read)

type Guess = [Square]

type Dictionary = [String]

data ContainsType = AtLeast | Exactly
    deriving (Show, Eq)

newtype Index = Ix Int
    deriving (Show, Eq)

data Filter
    = InSquare Index Char
    | NotInSquare Index Char
    | Contains ContainsType Int Char
    deriving (Show, Eq)

incContains :: Char -> [Filter] -> [Filter]
incContains c = map go
    where
        go (Contains t n c') =
            Contains t (if c == c' then n + 1 else n) c'
        go x = x

isContainsFilter :: Char -> Filter -> Bool
isContainsFilter c (Contains _ _ c') = c == c'
isContainsFilter _ _                 = False

noContains :: Char -> [Filter] -> Bool
noContains ch = not . any (isContainsFilter ch)

atLeastToExactly :: Char -> Filter -> Filter
atLeastToExactly c x@(Contains AtLeast n c')
    | c == c'        = Contains Exactly n c
    | otherwise      = x
atLeastToExactly _ x = x

getFilterChar :: Filter -> Char
getFilterChar (InSquare _ c)    = c
getFilterChar (NotInSquare _ c) = c
getFilterChar (Contains _ _ c)  = c

-- grey:
-- Contains AtLeast -> Contains Exactly
-- Contains Exactly -> id
-- if no "Contains" filters ++ Contains Exactly 0
grey :: Index -> Char -> [Filter] -> [Filter]
grey ix c fs
    | noContains c fs = Contains Exactly 0 c : fs'
    | otherwise = fs'
    where
        fs' = map (atLeastToExactly c) fs

-- yellow:
-- Contains x -> Contains (x + 1)
-- ++ NotInSquare
-- if no "Contains" filters ++ Contains AtLeast 1
yellow :: Index -> Char -> [Filter] -> [Filter]
yellow ix c fs
    | noContains c fs = Contains AtLeast 1 c : fs'
    | otherwise = fs'
    where
        fs' = NotInSquare ix c : incContains c fs

-- green:
-- InSquare
-- Contains x -> Contains (x + 1)
-- if no "Contains" filters ++ Contains AtLeast 1
green :: Index -> Char -> [Filter] -> [Filter]
green ix c fs
    | noContains c fs = Contains AtLeast 1 c : fs'
    | otherwise = fs'
    where fs' = InSquare ix c : incContains c fs

mkFilters :: Guess -> [Filter]
mkFilters = ($ [])
    . foldl' (.) id
    . zipWith mkFilter (map Ix [0..])
    where
        mkFilter :: Index -> Square -> [Filter] -> [Filter]
        mkFilter ix (Sq ch Grey)   = grey ix ch
        mkFilter ix (Sq ch Yellow) = yellow ix ch
        mkFilter ix (Sq ch Green)  = green ix ch

(.!!) :: [a] -> Index -> a
xs .!! (Ix n) = xs !! n

applyFilter :: String -> Filter -> Bool
applyFilter s (InSquare ix c) = c == s .!! ix
applyFilter s (NotInSquare ix c) = c /= s .!! ix
applyFilter s (Contains t n c) = length (filter (== c) s) `comp` n
    where
        comp = case t of
            AtLeast -> (>=)
            Exactly -> (==)

allFilters :: [Filter] -> String -> Bool
allFilters fs s = all (applyFilter s) fs

filterDict :: Dictionary -> Guess -> Dictionary
filterDict dict g = filter (allFilters $ mkFilters g) dict

randomWord :: Dictionary -> String
randomWord dict = dict !! ix
    where
        ix = fst $ randomR (0, length dict - 1) $ mkStdGen 1337

toColour :: Char -> Colour
toColour '0' = Grey
toColour '1' = Yellow
toColour '2' = Green
toColour _   = Grey

data Tree a = Root [Tree a] | Node a [Tree a]
    deriving Show

type Trie = Tree Char

nodeHasChar :: Char -> Trie -> Bool
nodeHasChar _ (Root _)    = False
nodeHasChar c (Node c' _) = c == c'

charFromNode :: Trie -> Char
charFromNode (Node c _) = c
charFromNode _          = error "No char in root"

recursiveInsert :: (a -> a) -> (a -> Bool) -> a -> [a] -> [a]
recursiveInsert _ _ def [] = [def]
recursiveInsert f check def (x:xs)
    | check x   = f x : xs
    | otherwise = x : recursiveInsert f check def xs

appChildren :: ([Trie] -> [Trie]) -> Trie -> Trie
appChildren f (Root ts)   = Root $ f ts
appChildren f (Node c ts) = Node c $ f ts

-- Note this approach only works as we know all words are exactly 5 characters
-- Therefore we do not need to consider a word being a substring of another.
-- If that were possible we would need to insert some sort of empty node into
-- the children of a node if it was the final node of a valid word.
insert :: String -> Trie -> Trie
insert "" = id
insert w@(c:cs) = appChildren $
    recursiveInsert (insert cs) (nodeHasChar c) (insert cs $ Node c [])

mkTrie :: Dictionary -> Trie
mkTrie = foldl' (flip insert) $ Root []

collapseTrie :: Trie -> Dictionary
collapseTrie (Root ts)   = concatMap collapseTrie ts
collapseTrie (Node c []) = [c : ""]
collapseTrie (Node c ts) = map (c :) $ concatMap collapseTrie ts

numChildren :: Trie -> Int
numChildren (Root ts)   = length ts
numChildren (Node _ ts) = length ts

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy $ \x y -> compare (f x) (f y)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy $ \x y -> compare (f x) (f y)

getChildren :: Trie -> [Trie]
getChildren (Root ts)   = ts
getChildren (Node _ ts) = ts

labelNumChildren :: Trie -> Tree (Char, Int)
labelNumChildren (Root ts)   = Root $ map labelNumChildren ts
labelNumChildren (Node c ts) = Node (c, length ts) $ map labelNumChildren ts

collapseLabelledTrie :: Tree (Char, Int) -> [(String, Int)]
collapseLabelledTrie (Root ts)        = concatMap collapseLabelledTrie ts
collapseLabelledTrie (Node (c, n) []) = [(c : "", n)]
collapseLabelledTrie (Node (c, n) ts) = map (bimap (c :) (+ n)) $
    concatMap collapseLabelledTrie ts

pickMostPopular :: Trie -> String
pickMostPopular = fst . maximumOn snd . collapseLabelledTrie . labelNumChildren

pickLeastPopular :: Trie -> String
pickLeastPopular = fst . minimumOn snd . collapseLabelledTrie . labelNumChildren

countLeaves :: Trie -> Int
countLeaves (Root ts)   = sum $ map countLeaves ts
countLeaves (Node c []) = 1
countLeaves (Node _ ts) = sum $ map countLeaves ts

isEmpty :: Trie -> Bool
isEmpty = null . getChildren

makeGuess :: Dictionary -> IO ()
makeGuess [] =
    putStrLn "Can't think of any more words :("
makeGuess [answer] =
    putStrLn $ "Answer is: " <> answer
makeGuess dict = do
    putStrLn $ "Selecting word from "
        <> show (length dict)
        <> " possibilities..."
    let word = pickMostPopular $ mkTrie dict
    putStrLn $ "Guess: " <> word
    result :: [Colour] <- map toColour <$> getLine
    let guess = zipWith Sq word result
    makeGuess $ filterDict dict guess

main :: IO ()
main = do
    dict <- lines <$> readFile "wordle.txt"
    makeGuess dict
