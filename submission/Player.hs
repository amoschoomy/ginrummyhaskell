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
import Data.Ord
import GHC.Unicode


-- |show instance for Card
instance Show Card where
    show (Card Heart a)="Card Heart "++ show a
    show (Card Diamond a)="Card Diamond "++ show a
    show (Card Spade a)="Card Spade " ++ show a
    show (Card Club a)="Card Club "++show a

-- |show instance for suit
instance Show Suit where
    show Heart="Heart"
    show Spade="Spade"
    show Diamond="Diamond"
    show Club="Club"

-- |show instance for rank
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


-- |show instance for meld
instance Show Meld where
    show (Deadwood x)="Deadwood"++ " "++ show x        -- An unmelded card
    show (Set3 x y z)  ="Set3"++" "++show x ++" "++ show y ++" "++ show z    -- 3 cards of same rank different suit
    show (Set4 a b c d) ="Set 4"++" "++ show a ++" "++ show b ++" "++ show c++" "++show d-- 4 cards of same rank different suit
    show (Straight3 a b c)="Straight 3" ++" "++ show a ++" "++ show b ++ " "++show c  -- 3 cards of same suit, sequential ranks
    show (Straight4 a b c d)="Straight 4"++" "++show a ++" "++ show b ++" "++ show c++" "++show d -- 4 cards of same suit, sequential ranks
    show  (Straight5 a b c d e)="Straight 5"++" "++" "++show a ++" "++ show b ++" "++ show c++" "++show d++" "++show e -- 5 cards of same suit, sequential ranks


{-| memory datatype for storing state of the game
cardsPlayed= total cardsPlayed in the game already
opponentCard = opponent played cards in the game (whatever new cards is on discardPile==opponenent discards)
discardPile = cards in discard pile (whatever cards dumped by player or not taken is here)
 -}
data Memory=Memory{cardsPlayed::Int,opponentCard::[Card],discardPile::[Card]}


-- |function to update memory given a datatype
updateMemory :: Memory -> [Card] -> [Card] -> Memory
updateMemory memory newpile newdiscardpile =Memory{cardsPlayed=cardsPlayed memory+1,
opponentCard=newpile++opponentCard memory,
discardPile=newdiscardpile++ discardPile memory}


-- |function to split string at specified character
-- |Referenced from https://stackoverflow.com/a/4981265
wordsWhen :: (Char->Bool)->String->[String]
wordsWhen p s=case dropWhile p s of
    ""->[]
    s' -> w: wordsWhen p s''
        where (w,s'')=break p s'

{- Parser for Card Data type here

-}

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

parseAce :: Parser Rank
parseAce = string "Ace" >> pure Ace

parseTwo ::Parser Rank
parseTwo = string "Two" >> pure Two

parseThree ::Parser Rank
parseThree=string "Three" >> pure Three

parseFour :: Parser Rank
parseFour = string "Four" >> pure Four

parseFive :: Parser Rank
parseFive = string "Five" >> pure Five

parseSix :: Parser Rank
parseSix = string "Six" >> pure Six

parseSeven :: Parser Rank
parseSeven = string "Seven" >>pure Seven

parseEight :: Parser Rank
parseEight = string "Eight" >> pure Eight

parseNine :: Parser Rank
parseNine= string "Nine" >> pure Nine

parseTen :: Parser Rank
parseTen = string "Ten" >> pure Ten

parseJack :: Parser Rank
parseJack=string "Jack" >> pure Jack

parseQueen :: Parser Rank
parseQueen = string "Queen" >> pure Queen

parseKing :: Parser Rank
parseKing = string "King" >> pure King


parseRank :: Parser Rank
parseRank= parseAce ||| parseTwo ||| parseThree ||| parseFour ||| parseFive 
        ||| parseSix ||| parseSeven ||| parseEight ||| parseNine ||| parseTen 
        ||| parseJack ||| parseQueen ||| parseKing

      
-- |Card Parser
parseCard :: Parser Card
parseCard = do 
    _ <- string "Card"
    _ <- space
    s <- parseSuit
    _ <- space
    Card s <$> parseRank
        
-- |Parse List of Cards
parselistofcards :: Parser [Card]
parselistofcards=list parseCard



{- helper codes below referenced from Tutorial 11-}
list :: Parser a -> Parser [a]
list p = (do
    first <- p
    rest <- list p
    pure (first:rest)) ||| pure []
    

space :: Parser Char
space = satisfy isSpace

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P func
    where
        func a = if a /= "" && f (head a) then parse character a else parse (unexpectedCharParser $ head a) ""


string :: String -> Parser String
string = traverse is

---------------------------------------------------------------------------------------------------------------

{- This block is to handle conversions of string into memory-}


--My memory data type when parsed into a string: "Int|String|String"
parsenumbercardsPlayed :: String ->Maybe(Int,String)
parsenumbercardsPlayed =readInt

getnumbercardsPlayed :: Maybe(Int,String)-> Int
getnumbercardsPlayed (Just (a,_))=a
getnumbercardsPlayed Nothing=0

getRemainingString :: Maybe(Int,String)->String
getRemainingString (Just(_,a))=a
getRemainingString Nothing=""

runParser :: ParseResult a -> Maybe a
runParser (Result _ a)=Just a
runParser (Error _)=Nothing




getResultsfromParser :: Maybe [Card] -> [Card]
getResultsfromParser (Just a)=a
getResultsfromParser Nothing=[]

-- |compile string into memory. uses parser, runParser and the functions below
compileStringintoMemory :: String -> Memory
compileStringintoMemory x= Memory{cardsPlayed=getnumbercardsPlayed(readInt x),
opponentCard=getResultsfromParser (runParser(parse parselistofcards(getOpponentPile(getRemainingString (readInt x))))),
discardPile=getResultsfromParser (runParser(parse parselistofcards(getDiscardPile(getRemainingString (readInt x)))))}


getOpponentPile :: String -> String
getOpponentPile "" =""
getOpponentPile "|"=""
getOpponentPile x =if null(wordsWhen (=='|')x)then ""else head (wordsWhen (=='|') x)

getDiscardPile :: String -> String
getDiscardPile x=if null (wordsWhen (=='|')x) then "" else last (wordsWhen (=='|')x)

-----------------------------------------------------------------------------------------------------------------

{-This block is to convert memory to the string in the format
    memory="cardsPlayed|opponentCard|discardPile"-}

memorytoString :: Memory -> String
memorytoString x=show(cardsPlayed x)++"|"++opponentCardtoString x++"|"++discardPiletoString x

opponentCardtoString :: Memory ->String
opponentCardtoString x=concatMap show (opponentCard x)

discardPiletoString :: Memory -> String
discardPiletoString x=concatMap show (discardPile x)


------------------------------------------------------------------------------------------------------------------


pickCard :: ActionFunc
pickCard (Card f x) score Nothing Nothing hand --base case when at start of the game and start of round

    --If losing by 25, launch offensive strategy
    --NOTE: Don't think this will be executed at all since memory is only Nothing at the firs turn of each round 
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,memorytoString
     Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards then choose from discard pile
    -- rank of cards picked also must be less than 5, to reduce odds of high values deadwoods
    |sameAny rankofcard (Card f x) hand && x<=Five || ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})
    
    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[Card f x]})


pickCard (Card f x) score (Just mem)  (Just Stock) hand --opponent choose to stock, other than first round
    --if opponent chose from stock, we know that, previous card of Discard pile is not favourable to 
        
    --If losing by 25, launch offensive strategy in hope of winning back.
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =
        (Discard,memorytoString(updateMemory (compileStringintoMemory mem) [] []))
    
    --Else try for general strategy, if succeeds, take from discard pile
    |drawFromDiscardGeneralStrat (Card f x) hand (Just mem) = 
        (Discard,memorytoString(updateMemory(compileStringintoMemory mem)[] []))

    --else choose from stock
    |otherwise=(Stock,memorytoString(updateMemory(compileStringintoMemory mem)[] [Card f x]))


pickCard (Card f x) score (Just mem) (Just Discard) hand--opponent choose to discard
    -- if opponent choose to discard
    -- we should prevent any more same rank or ranks(same suit) around being taken in the pile.

    --by checking top of discard pile, if same rank or ranks between the same suit,store it into opponentCard, take it

    --If losing by 25, ignore memory and use offensive strategy
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =
        (Discard,memorytoString(updateMemory (compileStringintoMemory mem) 
        (getLastCardofDiscardPilefromMemory (compileStringintoMemory mem)) []))

    --check if player can follow general strategy and check if ranks between same suit present. either true, then discard
    | drawFromDiscardGeneralStrat (Card f x) hand (Just mem) || ranksInBetweenSameSuitPresent hand (Card f x) ||
     checkOpponentCardandDiscard hand (Card f x) =
        (Discard,memorytoString(updateMemory(compileStringintoMemory mem)
        (getLastCardofDiscardPilefromMemory (compileStringintoMemory mem)) []))

    --else stock
    | otherwise=(Stock,memorytoString(updateMemory(compileStringintoMemory mem)
    (getLastCardofDiscardPilefromMemory (compileStringintoMemory mem)) [Card f x]))


-- First turn,second player
pickCard (Card f x) score Nothing (Just Discard) hand   --If losing by 25, launch offensive strategy
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards then choose from discard pile
    |sameAny rankofcard (Card f x) hand || ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})

    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[Card f x]})

pickCard (Card f x) score (Just _) Nothing hand
  --If losing by 25, launch offensive strategy
    --NOTE: Don't think this will be executed at all since memory is only Nothing at the firs turn of each round 
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,memorytoString
     Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards then choose from discard pile
    -- rank of cards picked also must be less than 5, to reduce odds of high values deadwoods
    |sameAny rankofcard (Card f x) hand && x<=Five || ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})
    
    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[Card f x]})

pickCard (Card f x) score Nothing (Just Stock) hand
    | uncurry (-) score> -25 && offensiveStratforPicking (Card f x) hand =(Discard,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards then choose from discard pile
    |sameAny rankofcard (Card f x) hand || ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})

    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[Card f x]})

getLastCardofDiscardPilefromMemory :: Memory -> [Card]
getLastCardofDiscardPilefromMemory mem
    |null (discardPile mem)=[]
    |otherwise= [last (discardPile mem)]


checkOpponentCardandDiscard :: [Card]->Card->Bool
checkOpponentCardandDiscard [] _=False
checkOpponentCardandDiscard opp (Card a b)= not (null$ filterSameRank opp b) 
    || ranksInBetweenSameSuitPresent opp (Card a b)

-- | Offensive strategy for picking, ignores memory in hope of winning
offensiveStratforPicking :: Card -> [Card] -> Bool
offensiveStratforPicking (Card s r) hand 
    | r==Seven=True --Take Seven at all costs

    --https://www.gamecolony.com/gin_rummy_hands.shtml strategy taken from
    | length (filter(\x->x==Card Spade Five ||x==Card Heart Five ||x==Card Spade Six || x==Card Heart Six) hand)>1 &&
    Card s r==Card Spade Five ||Card s r==Card Heart Five ||Card s r==Card Spade Six 
    || Card s r==Card Heart Six=True

    | length(filter(\x->x==Card Spade Ten ||x==Card Spade Jack||x==Card Heart Jack) hand )>1 && 
    Card s r==Card Spade Ten ||Card s r==Card Spade Jack||Card s r==Card Heart Jack =True

    | length (filter(\x->x==Card Spade Six||x==Card Spade Eight||x==Card Heart Eight) hand)>1 &&
    Card s r==Card Spade Six||Card s r==Card Spade Eight||Card s r==Card Heart Eight=True

    |otherwise=False



ranksInBetweenSameSuitPresent :: [Card] -> Card -> Bool
ranksInBetweenSameSuitPresent  hand (Card f x)
    |not (null (filterRankLessThanOne (filterSameSuit hand f) x)) && not (null (filterRankMoreThanOne (filterSameSuit hand f) x))=True
    |otherwise=False

drawFromDiscardGeneralStrat ::Card -> [Card] -> Maybe String -> Bool
drawFromDiscardGeneralStrat (Card _ y) playerhand (Just playermemory)
    | length (filterSameRank (discardPile (compileStringintoMemory playermemory)) y)>=2=True
    | length (filterSameRank playerhand y)==2 = True --If player at hand can form a possible run, discard
    | length (filterSameRank (opponentCard (compileStringintoMemory playermemory)) y)>=2=True--if opponent can form a run at the discard pile, take it
    | otherwise = False
drawFromDiscardGeneralStrat (Card _ _) _ Nothing=False


filterSameSuit :: [Card] -> Suit -> [Card]
filterSameSuit cards suit=filter isSameSuit cards
    where isSameSuit (Card s _)= s==suit

filterRankLessThanOne :: [Card] -> Rank -> [Card]
filterRankLessThanOne cards rank=filter lessrank cards
    where lessrank (Card _ r)=ranktoPoints r-ranktoPoints rank == -1

filterRankMoreThanOne :: [Card] -> Rank -> [Card]
filterRankMoreThanOne cards rank=filter morerank cards
    where morerank (Card _ r)=ranktoPoints r-ranktoPoints rank==1


ranktoPoints :: Rank -> Int
ranktoPoints r =fromEnum r +1

filterSameRank :: [Card] -> Rank -> [Card]
filterSameRank cards rank = filter isSameRank cards
    where isSameRank (Card _ r)= r==rank


sameAny :: Eq b => (t -> b) -> t -> [t] -> Bool
sameAny f c =any ((== f c) . f)

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

    -- if losing by more than 25, and decks can form Gin and only if half the deck has been played. This is to maximise the deadwood score of opponent
    |canGin (delete (getRiskiestCard hand (Card f x)) hand) (Card f x) (cardsPlayed (compileStringintoMemory mem))  &&cardsPlayed (compileStringintoMemory mem)>26 && uncurry (-) score > -25 =(Action Gin (getRiskiestCard hand (Card f x)),"0||")
    
    --in normal circumstances, call Gin ASAP
    |canGin (delete (getRiskiestCard hand (Card f x)) hand) (Card f x)(cardsPlayed (compileStringintoMemory mem))&& uncurry (-) score > -25=(Action Gin (getRiskiestCard hand (Card f x)),"0||")
    
    --if youre losing, call Knock ASAP
    |canKnock (delete (getRiskiestCard hand (Card f x))hand) (Card f x)(cardsPlayed (compileStringintoMemory mem)) && uncurry (-) score > -25=(Action Knock (getRiskiestCard hand (Card f x)),"0||")

    --try to match cards in opponent discards, and discard the same suits of opponent discards
    |length (filterSameSuit (discardPile (compileStringintoMemory mem)) f)-length (discardPile (compileStringintoMemory mem))<0=(Action Drop (chooseHighestValueCardofSameSuit hand (Just f)),"")
    
    --only knock, if less than half the deck played
    |canKnock(delete (getRiskiestCard hand (Card f x))hand) (Card f x)(cardsPlayed (compileStringintoMemory mem)) && cardsPlayed (compileStringintoMemory mem)<=26 && score/=(0,0)=(Action Knock (getRiskiestCard hand (Card f x)),"0||")
    
    --otherwise get maximum value at hand
    |otherwise=(Action Drop (maximum hand),"")


canKnock :: [Card]-> Card ->Int-> Bool
canKnock hand c@(Card _ _) a=(calculateDeadwoodScores $ (hand++[c]) \\(concat$filtermeld(listAllPossibleMelds hand c)))<10 && a>1

canGin :: [Card]-> Card ->Int-> Bool
canGin hand c@(Card _ _) a=(calculateDeadwoodScores $ (hand++[c]) \\(concat$filtermeld(listAllPossibleMelds hand c)))==0 && a>1

  

makeMelds :: MeldFunc
makeMelds _ _ card=map formMelds (filtermeld (listAllMeld card))++formDeadwood card (concat (filtermeld (listAllMeld card)))


formDeadwood :: [Card] -> [Card] -> [Meld]
formDeadwood x y=map Deadwood (x\\ y)


filtermeld:: [[Card]]->[[Card]] 
filtermeld melds=nubBy (\x y-> exists x y) melds



appendmeldvalue ::[[Card]] -> [Int] ->[([Card],Int)]
appendmeldvalue (a:ax) (b:bx) =(a,b):appendmeldvalue ax bx
appendmeldvalue [] []=[]
appendmeldvalue [] (_:_)=[]
appendmeldvalue (_:_) []=[]


getlowestmeldvalue:: [([Card],Int)]->[Card]
getlowestmeldvalue []=[]
getlowestmeldvalue x=fst (minimumBy (comparing snd) x)


chooseHighestValueCardofSameSuit :: [Card] ->Maybe Suit ->  Card
chooseHighestValueCardofSameSuit hand (Just suit) = if not$ null (filter (\(Card s _)->s==suit) hand) then   maximum (filter (\(Card s _)->s==suit) hand) else maximum hand
chooseHighestValueCardofSameSuit hand Nothing=maximum hand


cardToPoints :: Card->Int
cardToPoints (Card _ rank) = fromEnum rank + 1

checkStraightMeld :: [Card] ->Bool
checkStraightMeld l@(Card s _:xs)= cardToPoints (last l)-cardToPoints (head l) == (length l - 1)&& all(\(Card x _)-> x==s) xs
checkStraightMeld []=False

checkSetMeld :: [Card] -> Bool
checkSetMeld (Card s r:xs) = all (\(Card x y) -> x /= s && r==y ) xs
checkSetMeld []=False

getRiskiestCard :: [Card] -> Card -> Card
getRiskiestCard [] (Card f x)=Card f x
getRiskiestCard hand (Card f x)
    |null (selectBestPossibleMelds (listAllPossibleMelds hand (Card f x)))=maximum hand
    |otherwise=maximum$maximumBy (comparing length) (map(`getDeadwoods` hand)(selectBestPossibleMelds (listAllPossibleMelds hand (Card f x))))


calculateDeadwoodScores :: [Card] ->Int
calculateDeadwoodScores hand =   sum  (map toPoints hand)


selectBestPossibleMelds :: [[Card]] -> [Meld]
selectBestPossibleMelds (x:xs)=formMelds x:selectBestPossibleMelds xs
selectBestPossibleMelds []=[]


getDeadwoods :: Meld -> [Card] -> [Card]
getDeadwoods m@(Straight3 a b c) (w:wy)=if a/=w && b/=w && c/=w then w:getDeadwoods m wy  else getDeadwoods m wy
getDeadwoods m@(Straight4 a b c d) (w:wy)=if a/=w && b/=w && c/=w && d/=w then w:getDeadwoods m wy   else getDeadwoods m wy
getDeadwoods m@(Straight5 a b c d e) (w:wy)=if a/=w && b/=w && c/=w && d/=w && e/=w then w:getDeadwoods m wy   else getDeadwoods m wy
getDeadwoods m@(Set3 a b c) (w:wy)=if a/=w && b/=w && c/=w then w:getDeadwoods m wy   else getDeadwoods m wy
getDeadwoods m@(Set4 a b c d) (w:wy)=if a/=w && b/=w && c/=w && d/=w then w:getDeadwoods m wy   else getDeadwoods m wy
getDeadwoods (Deadwood _) x=x
getDeadwoods _ []=[]





listAllPossibleMelds :: [Card] ->Card-> [[Card]]
listAllPossibleMelds hand card = filter  possibleMeld $ filter (\x->length x>=3 && length x<=5) (subsequences (sort $hand++[card]))


listAllMeld ::[Card]->[[Card]]
listAllMeld hand=filter  possibleMeld $ filter (\x->length x>=3 && length x<=5) (subsequences (sort $hand))


possibleMeld :: [Card] -> Bool
possibleMeld hand =checkSetMeld hand || checkStraightMeld hand


formMelds :: [Card] -> Meld
formMelds l@[a,b,c]= x a b c where
    x=if checkStraightMeld l then Straight3 else Set3
formMelds []=undefined
formMelds [x]=Deadwood x
formMelds l@[a,b,c,d]= x a b c d where
    x=if checkStraightMeld l then Straight4 else Set4
formMelds [a,b,c,d,e]= Straight5 a b c d e
formMelds (_:_)=undefined





-- |Check if elements in two list intersects, returns true if there is intersection occurs
-- |Referenced from https://stackoverflow.com/a/47004828/
exists :: Eq a => [a] -> [a] -> Bool
exists x y = or $ (==) <$> x <*> y