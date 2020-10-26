-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Prelude
import Rummy.Rules
import Data.List

--Data Structure for PlayerMemory
data PlayerMemory=PlayerMemory ([Card],[Card])

-- Opponent chosen cards up to that turn
-- discard pile
-- Opponenet cards predicted supposed to be taken

--Functions will take Maybe String as parameter.
--We need to parse the string back into our datatype to be used

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.

-- type ActionFunc
--   = Card            -- ^ card on top of the discard pile
--   -> (Score, Score) -- ^ scores of (player, opponent) as of last round
--   -> Maybe String
--   -- ^ player's memory, on first player turn in the first round it will be Nothing
--   -> Maybe Draw -- ^ opponent's chosen action, on first game turn it will be Nothing
--   -> [Card]     -- ^ the player's hand
--   -> (Draw, String) -- ^ which pile did the player chose to draw from

--TODO: Parse String to PlayerMemory vice versa so pickCard function can be complete
-- pickCard :: ActionFunc
-- pickCard (Card f x) score Nothing Nothing hand --base case when at start of the game
--     | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,"()")
--     |sameAny rankofcard (Card f x) hand || ranksInBetweenSameSuitPresent hand (Card f x) =(Discard,"()")
--     |otherwise=(Stock,"()")

-- pickCard (Card f x) score (Just mem)  (Just Stock) hand --opponent choose to stock
--     --if opponet chose from stock, we know that, previous card of Discard pile is not favourable to opponent
--     --TODO: parse string to player memory type
--     | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,"()")
--     |drawFromDiscardGeneralStrat (Card f x) hand (Just mem) = (Discard,"()")
--     |otherwise=(Stock,"()")
-- pickCard (Card f x) score (Just mem) (Just Discard) hand--opponent choose to discard
--     -- if opponent choose to discard, we should prevent any more same rank or ranks(same suit) around being taken in the pile.
--     | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,"()")
--     | drawFromDiscardGeneralStrat (Card f x) hand (Just mem) || ranksInBetweenSameSuitPresent hand (Card f x) =(Discard,"()")
--     | otherwise=(Stock,"()")
-- pickCard (Card _ _)_  _ _ _=undefined

offensiveStratforPicking :: Card -> [Card] -> Bool
offensiveStratforPicking (Card s r) hand 
    | r==Seven=True --Take Seven at all costs

    --https://www.gamecolony.com/gin_rummy_hands.shtml strategy taken from
    | length (filter(\x->x==Card Spade Five ||x==Card Heart Five ||x==Card Spade Six || x==Card Heart Six) hand)>1 &&
    Card s r==Card Spade Five ||Card s r==Card Heart Five ||Card s r==Card Spade Six || Card s r==Card Heart Six=True

    | length(filter(\x->x==Card Spade Ten ||x==Card Spade Jack||x==Card Heart Jack) hand )>1 && 
    Card s r==Card Spade Ten ||Card s r==Card Spade Jack||Card s r==Card Heart Jack =True

    | length (filter(\x->x==Card Spade Six||x==Card Spade Eight||x==Card Heart Eight) hand)>1 &&
    Card s r==Card Spade Six||Card s r==Card Spade Eight||Card s r==Card Heart Eight=True

    |otherwise=False



ranksInBetweenSameSuitPresent :: [Card] -> Card -> Bool
ranksInBetweenSameSuitPresent  hand (Card f x)
    |not (null (filterRankLess (filterSameSuit hand f) x)) && not (null (filterRankMore (filterSameSuit hand f) x))=True
    |otherwise=False

drawFromDiscardGeneralStrat ::Card -> [Card] -> Maybe PlayerMemory -> Bool
drawFromDiscardGeneralStrat (Card x y) playerhand (Just playermemory)
    | not(null(filterSameRank (getDiscardPilefromMemory (Just playermemory)) y))=True
    | length (filterSameRank playerhand y)==2 = True --If player at hand can form a possible run, discard
    | length (filterSameRank (getOpponentHandfromMemory (Just playermemory)) y)>1=True --if opponent can form a run at the discard pile, take it
    | otherwise = False
drawFromDiscardGeneralStrat (Card x y) playerhand Nothing=False



getOpponentHandfromMemory::Maybe PlayerMemory ->[Card]
getOpponentHandfromMemory (Just (PlayerMemory(a,b)))=a
getOpponentHandfromMemory Nothing=[]

getDiscardPilefromMemory::Maybe PlayerMemory -> [Card]
getDiscardPilefromMemory (Just (PlayerMemory(a,b)))=b
getDiscardPilefromMemory Nothing= []

gDPfM:: PlayerMemory -> [Card]
gDPfM (PlayerMemory (a,b))=b

gOHfM :: PlayerMemory -> [Card]
gOHfM (PlayerMemory (a,b))=a

filterSameSuit :: [Card] -> Suit -> [Card]
filterSameSuit cards suit=filter isSameSuit cards
    where isSameSuit (Card s _)= if s==suit then True else False

filterRankLess :: [Card] -> Rank -> [Card]
filterRankLess cards rank=filter isSameRank cards
    where isSameRank (Card _ r)=if r<rank then True else False

filterRankMore :: [Card] -> Rank -> [Card]
filterRankMore cards rank=filter isSameRank cards
    where isSameRank (Card _ r)=if r>rank then True else False

filterSameRank :: [Card] -> Rank -> [Card]
filterSameRank cards rank = filter isSameRank cards
    where isSameRank (Card _ r)=if r==rank then True else False

sameAny :: Eq b => (t -> b) -> t -> [t] -> Bool
sameAny f c xs = any (== f c) (map f xs)

rankofcard :: Card -> Rank
rankofcard (Card _ r) = r


-- type PlayFunc
--   = Card              -- ^ picked card
--   -> (Score, Score)   -- ^ scores of (player, opponent) as of last round
--   -> String           -- ^ the player's memory
--   -> [Card]           -- ^ the player's hand
--   -> (Action, String) -- ^ the player's chosen card and new state

-- playCard :: PlayFunc
-- playCard (Card f x) score mem hand
--     | 
-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.

-- type MeldFunc
--   = String  -- ^ the player's memory
--   -> [Card] -- ^ cards in player's hand
--   -> [Meld] -- ^ elected melds
-- makeMelds :: MeldFunc
-- makeMelds x y=[Deadwood (Card Heart Ace)]


--Defensive strategy -- don't knock, try for gin.  If opponent discard a Card- check the suit. Dont make knocks late in the game
-- Offensive strategy, knock early, form multiple melds, only go for gin if half the deck is played, bait opponeents
--discarding a specific card you need for the sequence, discard the same value card but another suit and they probably discard your card

checkPercentageofCardsPlayed:: PlayerMemory -> Int
checkPercentageofCardsPlayed mem=((length (gDPfM mem)+ length (gOHfM mem) + 10 + 10) `div` 52) * 100


checkOpponentSuits :: PlayerMemory -> [Suit]
checkOpponentSuits mem =getSuitfromListofCards (gOHfM  mem)

checkDiscardPileSuits :: PlayerMemory -> [Suit]
checkDiscardPileSuits mem= getSuitfromListofCards (gDPfM mem)

getSuitfromListofCards :: [Card] -> [Suit]
getSuitfromListofCards ((Card s r):xs)=s:getSuitfromListofCards xs
getSuitfromListofCards []=  []




-- Don't knock, try to form melds for gin, only discard opponents discarded suits/ranks, discard high value cards first
-- defensivePlay :: Card -> String -> [Card] ->Action
-- defensivePlay (Card x y) mem  hand=
--     let opp= checkOpponentSuits mem

-- chooseHighestValueCardfromDeck ::[Card] -> Card
-- chooseHighestValueCardfromDeck= maximum



chooseHighestValueCardofSameSuit :: [Card] ->Suit ->  Card
chooseHighestValueCardofSameSuit hand suit = maximum(filter (\(Card s r)->s==suit) hand)



same :: Eq b => (t -> b) -> t -> [t] -> Bool
same f c xs = all (== f c) (map f xs)


-- data Meld =
--       Deadwood Card            -- An unmelded card
--     | Set3 Card Card Card      -- 3 cards of same rank different suit
--     | Set4 Card Card Card Card -- 4 cards of same rank different suit
--     | Straight3 Card Card Card -- 3 cards of same suit, sequential ranks
--     | Straight4 Card Card Card Card -- 4 cards of same suit, sequential ranks
--     | Straight5 Card Card Card Card Card -- 5 cards of same suit, sequential rank


-- loc=[Card Heart Five, Card Heart Six,Card Diamond Four, Card Club Seven, Card Spade King,Card Spade Four,Card Club Five]

possibleMeldLengths :: [Card] -> [[Card]]
possibleMeldLengths hand= filter (\x->length x>=2 && length x<=5) (subsequences hand)

checkStraightMeld :: [Card] ->Bool
checkStraightMeld list@(Card s r:xs)= toPoints (last list) - toPoints (head list)== length list-1 && all(\(Card x y)-> x==s) xs
checkStraightMeld []=False

checkSetMeld :: [Card] -> Bool
checkSetMeld (Card s r:xs) = all (\(Card x y) -> x /= s && r==y ) xs
checkSetMeld []=False

listAllPossibleMelds :: [[Card]] -> [[Card]]
listAllPossibleMelds =filter (\x->checkStraightMeld x || checkSetMeld x)

getDeadwoods :: [Card] ->[Card] -> [Card]
getDeadwoods meld hand = hand \\ meld

--We need to form melds, such that each melds are uniquely composed of diffeent cards.


calculateDeadwoodScores :: [Card] ->Int
calculateDeadwoodScores hand =   sum  (map toPoints hand)

-- [[Card]] -> Int

--Out of all the deadwood combinations, we have to find unique deadwoods that yield the least total deadwood score

compareList :: (Eq a) => [a] -> [a] -> Bool
compareList a = null . intersect a

-- getBestMeldCombos:: [[Card]] -> [[Card]]
-- getBestMeldCombos hand=  filter (\x-> \y ->compareList x y) hand
-- getBestMeldCombos []=[]
-- getBestMeldCombos [[]]=[]
-- getBestMeldCombos [x]= [x]

-- testCard=[[Card Heart Four, Card Heart Five, Card Heart Jack],[Card Diamond Ace, Card Spade Queen,Card Club Four],[Card Heart Six,Card Heart King, Card Heart Ten]]