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
import Data.Function
import Control.Applicative
import Parser.Instances
import Data.Ord
import Control.Monad
import GHC.Unicode


-- |show instance for Card
instance Show Card where
    show (Card Heart a)="CH"++ show a
    show (Card Diamond a)="CD"++ show a
    show (Card Spade a)="CS" ++ show a
    show (Card Club a)="CC"++show a

-- |show instance for suit
instance Show Suit where
    show Heart="H"
    show Spade="S"
    show Diamond="D"
    show Club="C"

-- |show instance for rank
instance Show Rank where
    show Ace="A"
    show Two="2"
    show Three="3"
    show Four="4"
    show Five="5"
    show Six="6"
    show Seven="7"
    show Eight="8"
    show Nine="9"
    show Ten="10"
    show Jack="J"
    show Queen="Q"
    show King="K"


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
data Memory=Memory{cardsPlayed::Int,opponentCard::[Card],discardPile::[Card]} deriving Show


-- |function to update memory given a datatype
updateMemory :: Memory -> [Card] -> [Card] -> Memory
updateMemory memory newpile newdiscardpile =Memory{cardsPlayed=cardsPlayed memory+1,
opponentCard=opponentCard memory++newpile,
discardPile=discardPile memory++newdiscardpile}


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
parseHeart = string "H" >> pure Heart

parseDiamond :: Parser Suit
parseDiamond= string "D" >> pure Diamond

parseClub :: Parser Suit
parseClub = string "C" >> pure Club

parseSpade:: Parser Suit
parseSpade = string "S" >> pure Spade

parseSuit :: Parser Suit
parseSuit = parseDiamond ||| parseClub ||| parseSpade ||| parseHeart

parseAce :: Parser Rank
parseAce = string "A" >> pure Ace

parseTwo ::Parser Rank
parseTwo = string "2" >> pure Two

parseThree ::Parser Rank
parseThree=string "3" >> pure Three

parseFour :: Parser Rank
parseFour = string "4" >> pure Four

parseFive :: Parser Rank
parseFive = string "5" >> pure Five

parseSix :: Parser Rank
parseSix = string "6" >> pure Six

parseSeven :: Parser Rank
parseSeven = string "7" >>pure Seven

parseEight :: Parser Rank
parseEight = string "8" >> pure Eight

parseNine :: Parser Rank
parseNine= string "9" >> pure Nine

parseTen :: Parser Rank
parseTen = string "10" >> pure Ten

parseJack :: Parser Rank
parseJack=string "J" >> pure Jack

parseQueen :: Parser Rank
parseQueen = string "Q" >> pure Queen

parseKing :: Parser Rank
parseKing = string "K" >> pure King


parseRank :: Parser Rank
parseRank= parseAce ||| parseTwo ||| parseThree ||| parseFour ||| parseFive 
        ||| parseSix ||| parseSeven ||| parseEight ||| parseNine ||| parseTen 
        ||| parseJack ||| parseQueen ||| parseKing

      
-- |Card Parser
parseCard :: Parser Card
parseCard = do 
    _ <- string "C"
    s <- parseSuit
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

-- |Parse number of cards played
parsenumbercardsPlayed :: String ->Maybe(Int,String)
parsenumbercardsPlayed =readInt

-- | Get number of cards played
getnumbercardsPlayed :: Maybe(Int,String)-> Int
getnumbercardsPlayed (Just (a,_))=a
getnumbercardsPlayed Nothing=0

-- | get remaining string after parsimg from readInt
getRemainingString :: Maybe(Int,String)->String
getRemainingString (Just(_,a))=a
getRemainingString Nothing=""

-- | Run the parser and return its value in Maybe
runParser :: ParseResult a -> Maybe a
runParser (Result _ a)=Just a
runParser (Error _)=Nothing



-- | get results from parser after maybe. Results in a list
getResultsfromParser :: Maybe [a] -> [a]
getResultsfromParser (Just a)=a
getResultsfromParser Nothing=[]

-- |compile string into memory. uses parser, runParser and the functions below
compileStringintoMemory :: String -> Memory
compileStringintoMemory x= Memory{cardsPlayed=getnumbercardsPlayed(readInt x),
opponentCard=getResultsfromParser 
(runParser(parse parselistofcards(getOpponentPile(getRemainingString (readInt x))))),
discardPile=getResultsfromParser 
(runParser(parse parselistofcards(getDiscardPile(getRemainingString (readInt x)))))}

-- | get string of opponent pile of memory string
-- | seperated by | |
getOpponentPile :: String -> String
getOpponentPile "" =""
getOpponentPile "|"=""
getOpponentPile x =if null(wordsWhen (=='|')x)then ""else head (wordsWhen (=='|') x)

-- | get string of discard Pile of memory string 
getDiscardPile :: String -> String
getDiscardPile x=if null (wordsWhen (=='|')x) then "" else last (wordsWhen (=='|')x)

-----------------------------------------------------------------------------------------------------------------

{-This block is to convert memory to the string in the format
    memory="cardsPlayed|opponentCard|discardPile"-}

-- | convert memory to string
memorytoString :: Memory -> String
memorytoString x=show(cardsPlayed x)++"|"++opponentCardtoString x++"|"++discardPiletoString x

opponentCardtoString :: Memory ->String
opponentCardtoString = show <=< opponentCard

discardPiletoString :: Memory -> String
discardPiletoString =show <=< opponentCard


------------------------------------------------------------------------------------------------------------------
{-This block is for extracting values from memory-}


-- |Get last discardpile of memory, returns empty list if null
getLastCardofDiscardPilefromMemory :: Memory -> [Card]
getLastCardofDiscardPilefromMemory mem
    |null (discardPile mem)=[]
    |otherwise= [last (discardPile mem)]

----------------------------------------------------------------------------------------------------------------
--This block is for helper functions for pickCard,playCard,makeMeld


-- | Check opponentCard from Memory and Discard to seen if there is similarities of discard and opponentCards
-- |  helps to decide take from stock or discardPile
checkOpponentCardandDiscard :: [Card]->Card->Bool
checkOpponentCardandDiscard [] _=False
checkOpponentCardandDiscard opp (Card a b)= not (null$ filterSameRank opp b) 
    || ranksInBetweenSameSuitPresent opp (Card a b)

-- | Offensive strategy for picking, ignores memory in hope of winning
offensiveStratforPicking :: Card -> [Card] -> Bool
offensiveStratforPicking (Card s r) hand 
    | r==Seven=True --Take Seven at all costs

    -- strategy taken from https://www.gamecolony.com/gin_rummy_hands.shtml
    | length (filter(\x->x==Card Spade Five ||x==Card Heart Five ||x==Card Spade Six ||
     x==Card Heart Six) hand)>1 &&
    Card s r==Card Spade Five ||
    Card s r==Card Heart Five ||Card s r==Card Spade Six 
    || Card s r==Card Heart Six=True

    | length(filter(\x->x==Card Spade Ten ||x==Card Spade Jack||
    x==Card Heart Jack) hand )>1 && 
    Card s r==Card Spade Ten ||Card s r==Card Spade Jack
    ||Card s r==Card Heart Jack =True

    | length (filter(\x->x==Card Spade Six||x==Card Spade Eight||x==Card Heart Eight) hand)>1 &&
    Card s r==Card Spade Six||Card s r==Card Spade Eight||Card s r==Card Heart Eight=True

    -- if no good offensive cards, false
    |otherwise=False


-- |Check if ranks in between same suit is present within the cards
ranksInBetweenSameSuitPresent :: [Card] -> Card -> Bool
ranksInBetweenSameSuitPresent  hand (Card f x)
    |not (null (filterRankLessThanOne (filterSameSuit hand f) x)) && not (null (filterRankMoreThanOne (filterSameSuit hand f) x))=True
    |otherwise=False

-- | General strategy to choose which card to take. If true, then take from discard
drawFromDiscardGeneralStrat ::Card -> [Card] -> Maybe String -> Bool
drawFromDiscardGeneralStrat (Card _ y) playerhand (Just playermemory)
    | length (filterSameRank (discardPile (compileStringintoMemory playermemory)) y)>=2=False --dont take since opponent also dont want
    | length (filterSameRank playerhand y)==2 = True --If player at hand can form a possible run, discard
    | length (filterSameRank (opponentCard (compileStringintoMemory playermemory)) y)>=2=True--if opponent can form a run at the discard pile, take it
    | otherwise = False
drawFromDiscardGeneralStrat (Card _ _) _ Nothing=False

-- | Filter for same suit
filterSameSuit :: [Card] -> Suit -> [Card]
filterSameSuit cards suit=filter isSameSuit cards
    where isSameSuit (Card s _)= s==suit

-- | Filter cards that are rank less than one exactly
filterRankLessThanOne :: [Card] -> Rank -> [Card]
filterRankLessThanOne cards rank=filter lessrank cards
    where lessrank (Card _ r)=ranktoPoints r-ranktoPoints rank == -1

-- | Filter cards that are rank more than one exactly
filterRankMoreThanOne :: [Card] -> Rank -> [Card]
filterRankMoreThanOne cards rank=filter morerank cards
    where morerank (Card _ r)=ranktoPoints r-ranktoPoints rank==1

-- | Own method of checking ranks of Jack,Queen,King respective ranking
ranktoPoints :: Rank -> Int
ranktoPoints r =fromEnum r +1

-- | Filter cards of same rank given a rank
filterSameRank :: [Card] -> Rank -> [Card]
filterSameRank cards rank = filter isSameRank cards
    where isSameRank (Card _ r)= r==rank

-- | Returns true if there is any same elements
sameAny :: Eq b => (t -> b) -> t -> [t] -> Bool
sameAny f c =any ((== f c) . f)

-- | Get rank of card
rankofcard :: Card -> Rank
rankofcard (Card _ r) = r

-- | Get most frequent suit of a deck, returns nothing if empty deck
-- |Code referenced/inspired from https://stackoverflow.com/a/10183501
getMostFrequentSuit :: [Card]->Maybe Card
getMostFrequentSuit []=Nothing
getMostFrequentSuit deck =Just$(last $ maximumBy (comparing length) $ group $ sort deck)

-- | check if a hand can knock. Dead wood score must be <=10, and must be at least second turn onwards 
canKnock :: [Card]-> Card ->Int-> Bool
canKnock hand c@(Card _ _) a= calculateHandScores((hand ++ [c])\\ 
    (concat $filtermeld (listAllPossibleMelds hand c)))<= 10 && a>1

-- | check if can gin, deadwood must be zero and second turn onwards    
canGin :: [Card]-> Card ->Int-> Bool
canGin hand c@(Card _ _) a=calculateHandScores((hand ++ [c])\\ 
    (concat $filtermeld (listAllPossibleMelds hand c)))< 1 && a>1

-- | Form deadwoods given two decks
formDeadwood :: [Card] -> [Card] -> [Meld]
formDeadwood x y=map Deadwood (x\\ y)
--x here refers to the full hand
-- y here refers to the decks of melds

-- | filter melds so that non duplicate melds can be used only
filtermeld:: [[Card]]->[[Card]] 
filtermeld melds=nubBy exists (arrangeMeld melds (map calculateHandScores melds))
-- | Card to points. Takes in a card and convert to points
cardToPoints :: Card->Int
cardToPoints (Card _ rank) = fromEnum rank + 1

-- | Check for straight meld
checkStraightMeld :: [Card] ->Bool
checkStraightMeld l@(Card s _:xs)= cardToPoints (last l)-cardToPoints (head l) == (length l - 1)&& all(\(Card x _)-> x==s) xs
checkStraightMeld []=False

-- | Check for set meld
checkSetMeld :: [Card] -> Bool
checkSetMeld (Card s r:xs) = all (\(Card x y) -> x /= s && r==y ) xs
checkSetMeld []=False

-- | Append meld values given a list of integers corrospending to the points
appendmeldvalue ::[[Card]] -> [Int] ->[([Card],Int)]
appendmeldvalue (a:ax) (b:bx) =(a,b):appendmeldvalue ax bx
appendmeldvalue [] []=[]
appendmeldvalue [] (_:_)=[]
appendmeldvalue (_:_) []=[]

-- | Arrange  melds such that the biggest melds value will be formed into melds first
arrangeMeld :: [[Card]] -> [Int] -> [[Card]]
arrangeMeld melds scores=reverse$deconstructmeld$sortBy (\(_,a) (_,b)->compare a b) 
    (appendmeldvalue melds scores) 


-- | get back the list of melds after getting the value
deconstructmeld :: [([Card],Int)] -> [[Card]]
deconstructmeld []=[]
deconstructmeld [([],_)]=[]
deconstructmeld ((a,_):xs)=a:deconstructmeld xs

-- | Choose highest value card of same suit, given a maybe suit
-- | If nothing is passed into,return maximum of hand
chooseHighestValueCardofSameSuit :: [Card] ->Maybe Suit ->  Card
chooseHighestValueCardofSameSuit hand (Just suit)
    |not (any (\ (Card s _) -> s == suit) hand) =maximum (filter (\(Card s _)->s==suit) hand)
    |otherwise= maximum hand
chooseHighestValueCardofSameSuit hand Nothing=maximum hand

-- | given a deck and a card, choose which card to drop
-- | logic based on what melds can formed with the deck
dropWhich :: [Card]->Card-> Card
dropWhich hand card
    |not (null$ sort hand \\ join(filtermeld(listAllPossibleMelds hand card)))=
        maximum$ hand \\   join(filtermeld(listAllPossibleMelds hand card))
    |otherwise=maximum hand

-- | get Suit of a card    
getSuit :: Card -> Suit
getSuit (Card f _)=f

-- | get rank of a card
getRank :: Card -> Rank
getRank (Card _ r)=r


-- |  given a hand calculateHandScores of all cards
calculateHandScores :: [Card] ->Int
calculateHandScores =   sum.map toPoints

-- | form list of melds
formlistofmelds :: [[Card]] -> [Meld]
formlistofmelds = map formMelds


-- | Given a meld and a hand, filter for deadwoods left from the hand
getDeadwoods :: Meld -> [Card] -> [Card]
getDeadwoods m@(Straight3 a b c) (w:wy)=
    if a/=w && b/=w && c/=w then w:getDeadwoods m wy  else getDeadwoods m wy
getDeadwoods m@(Straight4 a b c d) (w:wy)=
    if a/=w && b/=w && c/=w && d/=w then w:getDeadwoods m wy   else getDeadwoods m wy
getDeadwoods m@(Straight5 a b c d e) (w:wy)=
    if a/=w && b/=w && c/=w && d/=w && e/=w then w:getDeadwoods m wy   else getDeadwoods m wy
getDeadwoods m@(Set3 a b c) (w:wy)=
    if a/=w && b/=w && c/=w then w:getDeadwoods m wy   else getDeadwoods m wy
getDeadwoods m@(Set4 a b c d) (w:wy)=
    if a/=w && b/=w && c/=w && d/=w then w:getDeadwoods m wy   else getDeadwoods m wy
getDeadwoods (Deadwood _) x=x
getDeadwoods _ []=[]


-- |get Riskiest card
-- | There's four parameters, hand, risky_card formed,picked_card at hand, and opponentCard from memory
-- | check if the suit of the risky card selected previously is the same as suit at opponent memory
-- | else call dropWhich function to choose which card to drop other than the risky card selected previously
getRiskiestCard ::[Card]-> Card->Card -> Maybe Card -> Card
getRiskiestCard hand riskycard cardpicked  (Just (Card f _))=if getSuit riskycard==f then dropWhich (delete riskycard hand) cardpicked else riskycard
getRiskiestCard _ riskycard _ Nothing=riskycard




-- | list all possible melds from the deck and the card you picked too
listAllPossibleMelds :: [Card] ->Card-> [[Card]]
listAllPossibleMelds hand card = filter  possibleMeld $ filter (\x->length x>=3 && length x<=5) (subsequences (sort $hand++[card]))

-- | same but this is called without the pickedcard
-- | as the card is already in your deck
listAllMeld ::[Card]->[[Card]]
listAllMeld hand=filter  possibleMeld $ filter (\x->length x>=3 && length x<=5) (subsequences (sort $hand))

-- | given a deck, check if any melds can be formed
possibleMeld :: [Card] -> Bool
possibleMeld hand =checkSetMeld hand || checkStraightMeld hand


-- | given a deck of card, form melds
-- | due to how I call this functions, deadwoods can never be formed using this function
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
---------------------------------------------------------------------------------------------------------------
pickCard :: ActionFunc
pickCard (Card f x) score Nothing Nothing hand --base case when at start of the game and start of round

    --If losing by 20, launch offensive strategy
    --NOTE: Don't think this will be executed at all since memory is only Nothing at the firs turn of each round 
    | uncurry (-) score> -20 && offensiveStratforPicking (Card f x) hand =(Discard,memorytoString
     Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards
    --then choose from discard pile
    -- rank of cards picked also must be less than 5, to reduce odds of high values deadwoods
    |sameAny rankofcard (Card f x) hand && x<=Five || ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]})
    
    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[Card f x]})

--opponent choose to stock, other than first round, nothing can be added to memory because opponent chose from stock
pickCard (Card f x) score (Just mem)  (Just Stock) hand=

    --discard pile previous card not favourable to opponent so we need to check discardpile
    case getLastCardofDiscardPilefromMemory memory of
        --if no discardPile then

            --launch offensive strategy if trailing by 10
        [] |offense ->
            (Discard,memorytoString(updateMemory memory [] []))

            --general strategy normally
           |drawFromDiscardGeneralStrat (Card f x) hand (Just mem) ->
               (Discard,memorytoString(updateMemory memory [] [Card f x])) 
            --else get from stock
           |otherwise ->
                (Stock,memorytoString(updateMemory memory [] [Card f x]))

        --if discard pile avaliable..

            --check the suit or rank, if they are different it's safe to take but
            --first offensive strategy
        xs |offense ->
            (Discard,memorytoString(updateMemory memory [] []))
        

           |getSuit (last$xs)/=f || getRank(last$xs)/=x ->
            (Discard,memorytoString(updateMemory memory [] []))
            -- else get from stock
           |otherwise ->
               (Stock,memorytoString(updateMemory memory[] [Card f x]))
        where
            memory=compileStringintoMemory mem
            offense=uncurry (-) score> -10 && offensiveStratforPicking (Card f x) hand

--opponent choose to discard
pickCard (Card f x) score (Just mem) (Just Discard) hand

    --If losing by 10, ignore memory and use offensive strategy
    | uncurry (-) score> -10 && offensiveStratforPicking (Card f x) hand =
        (Discard,memorytoString(updateMemory memory last_discard []))

    --check if player can follow general strategy and check if ranks between same suit present. either true, then discard
    | drawFromDiscardGeneralStrat (Card f x) hand (Just mem) || 
    ranksInBetweenSameSuitPresent hand (Card f x) ||
     checkOpponentCardandDiscard hand (Card f x) =
        (Discard,memorytoString(updateMemory memory last_discard []))

    --else stock
    | otherwise=(Stock,memorytoString(updateMemory memory last_discard [Card f x]))
    where
        last_discard=getLastCardofDiscardPilefromMemory (compileStringintoMemory mem)
        memory=compileStringintoMemory mem

-- First turn,second playerm opponent discard
pickCard (Card f x) score Nothing (Just Discard) hand   
    --If losing by 10, launch offensive strategy
    | uncurry (-) score> -10 && offensiveStratforPicking (Card f x) hand =(Discard,newmemory)

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards then choose from discard pile
    |sameAny rankofcard (Card f x) hand || ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,newmemory)

    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[Card f x]})
    where newmemory=memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]}


pickCard (Card f x) score (Just _) Nothing hand
  --If losing by 10, launch offensive strategy
    --NOTE: Don't think this will be executed at all since memory is only Nothing at the firs turn of each round 
    | uncurry (-) score> -10 && offensiveStratforPicking (Card f x) hand =(Discard,newmemory)

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards then choose from discard pile
    -- rank of cards picked also must be less than 5, to reduce odds of high values deadwoods
    |sameAny rankofcard (Card f x) hand && x<=Five || 
    ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,newmemory)
    
    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[Card f x]})
    where newmemory=memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]}


--opponent chose from stock
pickCard (Card f x) score Nothing (Just Stock) hand
    | uncurry (-) score> -10 && offensiveStratforPicking (Card f x) hand =(Discard,newmemory)

    --check player hand and the card on top of discard pile
    -- such that if there is ranksInBetweenSameSuitPresent or any same rank of cards then choose from discard pile
    --card picked also must be rank less than Five
    |sameAny rankofcard (Card f x) hand && x<=Five || ranksInBetweenSameSuitPresent hand (Card f x) =
        (Discard,newmemory)

    --else, get from Stock
    |otherwise=(Stock,memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[Card f x]})
    where newmemory=memorytoString Memory{cardsPlayed=1,opponentCard=[],discardPile=[]}





playCard :: PlayFunc
playCard (Card f x) score mem hand

    --half the deck means 13 in memory
    -- if losing by more than 10, and decks can form Gin and only if half the deck has been played. This is to maximise the deadwood score of opponent
    |gin&&cardsPlayed memory>13 && uncurry (-) score > -10 =
        (Action Gin (risky_card frequent_suit),"0||")
    
    --in normal circumstances, call Gin ASAP and reset memory
    |gin=(Action Gin (risky_card frequent_suit),"0||")
    
    --if youre losing, call Knock ASAP and reset memory
    |knock && uncurry (-) score > -10=
        (Action Knock (risky_card frequent_suit),"0||")

    --only knock, if less than half the deck played(13 since card goes thru twice)
    |knock&& cardsPlayed (compileStringintoMemory mem)<=13 =
        (Action Knock (risky_card frequent_suit),"0||")
    
    --try to match cards in opponent discards, and discard the same suits of opponent discards
    |not$ null (discardPile memory)=(Action Drop highest_same_suit,
    memorytoString(updateMemory memory [][highest_same_suit]))    
    
    --otherwise get maximum value of the riskieest card at hand
    |otherwise=(Action Drop (risky_card frequent_suit),mem)
    where
        gin=canGin (delete (risky_card frequent_suit) hand)(Card f x)(cardsPlayed memory)

        knock=canKnock (delete (risky_card frequent_suit) hand)(Card f x)(cardsPlayed memory)

        memory=compileStringintoMemory mem

        risky_card=getRiskiestCard hand(dropWhich hand (Card f x)) (Card f x)

        frequent_suit=getMostFrequentSuit (opponentCard (compileStringintoMemory mem))

        discard_pile_suit=getSuit$last$sort(discardPile(compileStringintoMemory mem))

        highest_same_suit=chooseHighestValueCardofSameSuit hand (Just discard_pile_suit)


  

makeMelds :: MeldFunc
makeMelds _ _ card=map formMelds (filtermeld (listAllMeld card))++formDeadwood card (concat (filtermeld (listAllMeld card)))

