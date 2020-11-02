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
import Data.Maybe
import Control.Applicative
import Parser.Instances
import GHC.Unicode
import Data.Ord



instance Show Card where
    show (Card Heart a)="Card Heart "++ show a
    show (Card Diamond a)="Card Diamond "++ show a
    show (Card Spade a)="Card Spade " ++ show a
    show (Card Club a)="Card Club "++show a

instance Show Suit where
    show Heart="Heart"
    show Spade="Spade"
    show Diamond="Diamond"
    show Club="Club"

instance Show Rank where
    show Ace="Ace"
    show Two="Two"
    show Three="Three"
    show Four="Four"
    show Five="Five"
    show Six="Six"
    show Seven="Seven"
    show Eight="Eight"
    show Nine="Nine"
    show Ten="Ten"
    show Jack="Jack"
    show Queen="Queen"
    show King="King"

instance Show Meld where
    show (Deadwood x)="Deadwood"++ show x        -- An unmelded card
    show (Set3 x y z)  ="Set3"++show x ++ show y ++ show z    -- 3 cards of same rank different suit
    show (Set4 a b c d) ="Set 4"++ show a ++ show b ++ show c++show d-- 4 cards of same rank different suit
    show (Straight3 a b c)="Straight 3" ++ show a ++ show b ++ show c  -- 3 cards of same suit, sequential ranks
    show (Straight4 a b c d)="Straight 4"++show a ++ show b ++ show c++show d -- 4 cards of same suit, sequential ranks
    show  (Straight5 a b c d e)="Straight 5"++show a ++ show b ++ show c++show d++show e -- 5 cards of same suit, sequential ranks


data Memory=Memory{cardsPlayed::Int,opponentCard::String,discardPile::String} deriving Show

--cardsPlayed= total cardsPlayed in the game already
-- opponentCard = opponent played cards in the game (whatever new cards is on discardPile==opponenent discards)
-- discardPile = cards in discard pile (whatever cards dumped by player or not taken is here)



wordsWhen :: (Char->Bool)->String->[String]
wordsWhen p s=case dropWhile p s of
    ""->[]
    s' -> w: wordsWhen p s''
        where (w,s'')=break p s'

parsenumbercardsPlayed :: String ->Maybe(Int,String)
parsenumbercardsPlayed =readInt

getnumbercardsPlayed :: Maybe(Int,String)-> Int
getnumbercardsPlayed (Just (a,b))=a
getnumbercardsPlayed Nothing=0

getRemainingString :: Maybe(Int,String)->String
getRemainingString (Just(_,a))=a
getRemainingString Nothing=""


getOpponentPile :: String -> String
getOpponentPile x =head (wordsWhen (=='|') x)

getDiscardPile :: String -> String
getDiscardPile x=last (wordsWhen (=='|')x)


compileCardsPlayedMemorytoString :: Memory -> [Char]
compileCardsPlayedMemorytoString x= show (cardsPlayed x)

compileopponentCardMemorytoString :: Memory -> String
compileopponentCardMemorytoString =opponentCard

compileDiscardPiletoString :: Memory -> String
compileDiscardPiletoString =discardPile

compileStringintoMemory :: String -> Memory
compileStringintoMemory x= Memory{cardsPlayed=getnumbercardsPlayed(readInt x),
opponentCard=getOpponentPile(getRemainingString (readInt x)),
discardPile=getDiscardPile(getRemainingString (readInt x))}

updateMemory :: Memory -> String -> String -> Memory
updateMemory memory newpile newdiscardpile =Memory{cardsPlayed=cardsPlayed memory+1,
opponentCard=newpile++opponentCard memory,
discardPile=newdiscardpile++ discardPile memory}

memorytoString :: Memory -> String
memorytoString x=compileCardsPlayedMemorytoString x ++"|" ++compileopponentCardMemorytoString x ++"|"++ compileDiscardPiletoString x

parseHeart :: Parser Suit
parseHeart = string "Heart" >> pure Heart


parseDiamond :: Parser Suit
parseDiamond= string "Diamond" >> pure Diamond

parseClub :: Parser Suit
parseClub = string "Club" >> pure Club
parseSpade:: Parser Suit
parseSpade = string "Spade" >> pure Spade

parseSuit :: Parser Suit
parseSuit = parseDiamond ||| parseClub ||| parseSpade ||| parseHeart

string :: String -> Parser String
string = traverse is


runParser :: ParseResult a -> Maybe a
runParser (Result b a)=Just a
runParser (Error _)=Nothing










pickCard :: ActionFunc
pickCard (Card f x) score Nothing Nothing hand --base case when at start of the game

    --If losing by 25, launch offensive strategy
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,memorytoString Memory{cardsPlayed=1,opponentCard="",discardPile=""})

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards then choose from discard pile
    |sameAny rankofcard (Card f x) hand || ranksInBetweenSameSuitPresent hand (Card f x) =(Discard,memorytoString Memory{cardsPlayed=1,opponentCard="",discardPile=""})
    
    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard="",discardPile=show (Card f x)})


pickCard (Card f x) score (Just mem)  (Just Stock) hand --opponent choose to stock
    --if opponet chose from stock, we know that, previous card of Discard pile is not favourable to 
        
    --If losing by 25, launch offensive strategy in hope of winning back.
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,memorytoString(updateMemory (compileStringintoMemory mem) "" ""))
    
    --Else try for general strategy, if succeeds, take from discard pile
    |drawFromDiscardGeneralStrat (Card f x) hand (Just mem) = (Discard,memorytoString(updateMemory(compileStringintoMemory mem)"" ""))

    --else choose from stock
    |otherwise=(Stock,memorytoString(updateMemory(compileStringintoMemory mem)"" (show (Card f x))))


pickCard (Card f x) score (Just mem) (Just Discard) hand--opponent choose to discard
    -- if opponent choose to discard, we should prevent any more same rank or ranks(same suit) around being taken in the pile.

    --If losing by 25, ignore memory and use offensive strategy
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,memorytoString(updateMemory (compileStringintoMemory mem) "" ""))

    --check if player can follow general strategy and check if ranks between same suit present. either true, then discard
    | drawFromDiscardGeneralStrat (Card f x) hand (Just mem) || ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,memorytoString(updateMemory(compileStringintoMemory mem)"" ""))

    --else stock
    | otherwise=(Stock,memorytoString(updateMemory(compileStringintoMemory mem)"" (show (Card f x))))

    
--Only 3 possible conditions in game, so make this pattern matching undefined
pickCard (Card _ _)_  _ _ _=undefined


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

drawFromDiscardGeneralStrat ::Card -> [Card] -> Maybe String -> Bool
drawFromDiscardGeneralStrat (Card x y) playerhand (Just playermemory)
    | countnumberofRanks(getDiscardPile playermemory)(show y)>0=True
    | length (filterSameRank playerhand y)==2 = True --If player at hand can form a possible run, discard
    | countnumberofRanks (getOpponentPile playermemory) (show y)>1=True --if opponent can form a run at the discard pile, take it
    | otherwise = False
drawFromDiscardGeneralStrat (Card x y) playerhand Nothing=False


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


countnumberofRanks :: String ->String-> Int
countnumberofRanks x y=length (filter (== y) (words x))

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

playCard :: PlayFunc
playCard (Card f x) score mem hand
    |0 `elem` scoreofcard (delete (getRiskiestCard hand (Card f x)) hand) (Card f x)=(Action Gin (getRiskiestCard hand (Card f x)),"")
    |uncurry (>) score=(Action Drop (chooseHighestValueCardofSameSuit hand (mostFrequentSuit$opponentCard (compileStringintoMemory mem))),"")
playCard (Card _ _) _ _ _=undefined

scoreofcard:: [Card] -> Card -> [Int]
scoreofcard hand (Card f x)=map((sum . map toPoints) . (`calculateindscore` hand)) (selectBestPossibleMelds (listAllPossibleMelds hand (Card f x)))


    --    |0 `elem`map (sum . map toPoints)(map(\ x -> calculateindscore x hand)(selectBestPossibleMeld (listAllPossibleMelds hand (Card f x))))=(,)
    --    |0 `elem` (sum . map toPoints) . (`calculateindscore` hand)(selectBestPossibleMeld (listAllPossibleMelds hand (Card f x)))=(Action Gin (Card f x),"")
-- type MeldFunc
--   = String  -- ^ the player's memory
--   -> [Card] -- ^ cards in player's hand
--   -> [Meld] -- ^ elected melds
-- makeMelds :: MeldFunc
-- makeMelds x y=[Deadwood (Card Heart Ace)]

-- |0 `elem`( map sum((map.map) toPoints  (map(\x->calculateindscore x hand) (selectBestPossibleMelds(listAllPossibleMelds hand (Card f x))))))=(Action Gin (Card f x),"")


--Defensive strategy -- don't knock, try for gin.  If opponent discard a Card- check the suit. Dont make knocks late in the game
-- Offensive strategy, knock early, form multiple melds, only go for gin if half the deck is played, bait opponeents
--discarding a specific card you need for the sequence, discard the same value card but another suit and they probably discard your card


chooseHighestValueCardofSameSuit :: [Card] ->Maybe Suit ->  Card
chooseHighestValueCardofSameSuit hand (Just suit) = maximum(filter (\(Card s r)->s==suit) hand)
chooseHighestValueCardofSameSuit hand Nothing=maximum hand


-- loc=[Card Heart Five, Card Heart Six,Card Diamond Four, Card Club Seven, Card Spade King,Card Spade Four,Card Club Five]

cardToPoints :: Card->Int
cardToPoints (Card _ rank) = fromEnum rank + 1

checkStraightMeld :: [Card] ->Bool
checkStraightMeld list@(Card s r:xs)= cardToPoints (last list)-cardToPoints (head list) == (length list - 1)&& all(\(Card x y)-> x==s) xs
checkStraightMeld []=False

checkSetMeld :: [Card] -> Bool
checkSetMeld (Card s r:xs) = all (\(Card x y) -> x /= s && r==y ) xs
checkSetMeld []=False

getRiskiestCard :: [Card] -> Card -> Card
getRiskiestCard hand (Card f x)=maximum$maximumBy (comparing length) (map(`calculateindscore` hand)(selectBestPossibleMelds (listAllPossibleMelds hand (Card f x))))


mostFrequentSuit :: String -> Maybe Suit
mostFrequentSuit x=runParser (parse parseSuit (maximum(maximumBy (comparing length)(group.sort$words x))))
-- maximum(maximumBy (comparing length)(group.sort$words

getDeadwoods :: [Card] ->[Card] -> [Card]
getDeadwoods meld hand = hand \\ meld

--We need to form melds, such that each melds are uniquely composed of diffeent cards.


calculateDeadwoodScores :: [Card] ->Int
calculateDeadwoodScores hand =   sum  (map toPoints hand)


--Out of all the deadwood combinations, we have to find unique deadwoods that yield the least total deadwood score

-- selectBestPossibleMelds :: [[Card]] ->[Card]-> [Int]
-- selectBestPossibleMelds (x:xs) hand=reverse $ foldr(\ p -> (:) (calculateDeadwoodScores (getDeadwoods p hand))) [calculateDeadwoodScores (getDeadwoods x hand)] xs
-- selectBestPossibleMelds [] hand=[calculateDeadwoodScores(getDeadwoods hand hand)]

-- map (\x-> calculateindscore x [Card Heart Five,Card Heart Six,Card Heart Seven,Card Diamond King]) meld)

selectBestPossibleMelds :: [[Card]] -> [Meld]
selectBestPossibleMelds lol@(x:xs)=formMelds x:selectBestPossibleMelds xs
selectBestPossibleMelds []=[]


scoreCards:: [Card] -> Int
scoreCards x=sum (map toPoints x)




calculateindscore :: Meld -> [Card] -> [Card]
calculateindscore m@(Straight3 a b c) l@(w:wy)=if a/=w && b/=w && c/=w then w:calculateindscore m wy  else calculateindscore m wy
calculateindscore m@(Straight4 a b c d) l@(w:wy)=if a/=w && b/=w && c/=w && d/=w then w:calculateindscore m wy   else calculateindscore m wy
calculateindscore m@(Straight5 a b c d e) l@(w:wy)=if a/=w && b/=w && c/=w && d/=w && e/=w then w:calculateindscore m wy   else calculateindscore m wy
calculateindscore m@(Set3 a b c) l@(w:wy)=if a/=w && b/=w && c/=w then w:calculateindscore m wy   else calculateindscore m wy
calculateindscore m@(Set4 a b c d) l@(w:wy)=if a/=w && b/=w && c/=w && d/=w then w:calculateindscore m wy   else calculateindscore m wy
calculateindscore (Deadwood a) x=x
calculateindscore x []=[]





listAllPossibleMelds :: [Card] ->Card-> [[Card]]
listAllPossibleMelds hand card = filter  possibleMeld $ filter (\x->length x>=3 && length x<=5) (subsequences (sort $hand++[card]))

possibleMeld :: [Card] -> Bool
possibleMeld hand =checkSetMeld hand || checkStraightMeld hand

-- qcards=[Card Heart Nine, Card Heart Ten, Card Heart King, Card Diamond Ace, Card Diamond Ten, Card Club Ten]


formMelds :: [Card] -> Meld
formMelds l@[a,b,c]= x a b c where
    x=if checkStraightMeld l then Straight3 else Set3
formMelds []=undefined
formMelds [x]=Deadwood x
formMelds l@[a,b,c,d]= x a b c d where
    x=if checkStraightMeld l then Straight4 else Set4
formMelds l@[a,b,c,d,e]= Straight5 a b c d e
formMelds l@(x:xs)=undefined






--Referenced from https://stackoverflow.com/a/47004828/
exists :: Eq a => [a] -> [a] -> Bool
exists x y = or $ (==) <$> x <*> y


-- meld= Straight3 (Card Heart Nine) (Card Heart Ten) (Card Heart Queen)
