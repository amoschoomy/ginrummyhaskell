-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Data.List
import Prelude

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
pickCard :: ActionFunc
-- pickCard card memory drw hand=
pickCard (Card f x) Nothing Nothing hand =(pile,memory) where --base case when at start of the game
    pile=if sameAny rankofcard (Card f x) hand && length (filterRankLess (filterSameSuit hand f) x) /=0 && length (filterRankMore (filterSameSuit hand f) x) /=0 then Discard else Stock
    memory="Stock"
pickCard (Card f x) (Just l) (Just lol) hand=(pile,memory)  where --depends on player memory
    pile=Discard
    memory="Discard"
pickCard (Card _ _) _ _ _=undefined

cardsw=[Card Spade Four,Card Diamond Five,Card Heart Five, Card Spade Seven]

-- checkPossibleMelds :: [Card] -> Card -> Bool
-- checkPossibleMelds [] (Card x y)=[]
-- checkPossibleMelds (x:xs) (Card x y)=undefined


filterSameSuit :: [Card] -> Suit -> [Card]
filterSameSuit cards suit=filter isSameSuit cards
    where isSameSuit (Card s _)= if s==suit then True else False

filterRankLess :: [Card] -> Rank -> [Card]
filterRankLess cards rank=filter isSameRank cards
    where isSameRank (Card _ r)=if r<rank then True else False

filterRankMore :: [Card] -> Rank -> [Card]
filterRankMore cards rank=filter isSameRank cards
    where isSameRank (Card _ r)=if r>rank then True else False



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
playCard (Card f x) mem hand=(Action Drop (Card f x),"LOL")
-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.

-- type MeldFunc
--   = String  -- ^ the player's memory
--   -> [Card] -- ^ cards in player's hand
--   -> [Meld] -- ^ elected melds
makeMelds :: MeldFunc
makeMelds x y=[Deadwood (Card Heart Ace)]
