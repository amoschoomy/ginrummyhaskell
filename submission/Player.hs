-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Prelude
import Rummy.Rules


--Data Structure for PlayerMemory
data PlayerMemory=PlayerMemory ([Card],[Card])
    deriving(Show)

-- Opponent chosen cards up to that turn
-- discard pile
-- Opponenet cards predicted supposed to be taken

--Functions will take Maybe String as parameter.
--We need to parse the string back into our datatype to be used

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
pickCard :: ActionFunc
pickCard (Card f x) Nothing Nothing hand --base case when at start of the game
    |ranksInBetweenSameSuitPresent (hand) (Card f x) =(Discard,"()")
    |otherwise=(Stock,"()")
pickCard (Card f x) (Just mem)  (Just Stock) hand --opponent choose to stock
    --if opponet chose from stock, we know that, previous card of Discard pile is not favourable to opponent
    --TODO: parse string to player memory type
    |drawFromDiscardGeneralStrat (Card f x) hand (Just mem) = (Discard,"()")
    |otherwise=(Stock,"()")
pickCard (Card f x) (Just mem) (Just Discard) hand--opponent choose to discard
    -- if opponent choose to discard, we should prevent any more same rank or ranks(same suit) around being taken in the pile.
    | drawFromDiscardGeneralStrat (Card f x) hand (Just mem) || ranksInBetweenSameSuitPresent hand (Card f x) =(Discard,"()")
    | otherwise=(Stock,"()")
pickCard (Card _ _) _ _ _=undefined

cardsw=[Card Spade Four,Card Diamond Five,Card Heart Five, Card Spade Seven]


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
getDiscardPilefromMemory Nothing=[]


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

-- type ActionFunc
--   = Card          -- ^ card on top of the discard pile
--   -> Maybe String -- ^ player's memory, on first player turn it will be Nothing
--   -> Maybe Draw -- ^ opponent's chosen action, on first game turn it will be Nothing
--   -> [Card]       -- ^ the player's hand
--   -> (Draw, String) -- ^ which pile did the player chose to draw from
-- | This function is called once you have drawn a card, you need to decide
-- which action to call.


-- type PlayFunc
--   = Card              -- ^ last picked card
--   -> String           -- ^ the player's memory
--   -> [Card]           -- ^ the player's hand
--   -> (Action, String) -- ^ the player's chosen card and new state



playCard :: PlayFunc
playCard (Card f x) mem hand=(Action Knock (head hand),"LOL")
-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.

-- type MeldFunc
--   = String  -- ^ the player's memory
--   -> [Card] -- ^ cards in player's hand
--   -> [Meld] -- ^ elected melds
makeMelds :: MeldFunc
makeMelds x y=[Deadwood (Card Heart Ace)]
