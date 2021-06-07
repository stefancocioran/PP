{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List()

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

blankCell :: Char
blankCell = ' '

hunterCell :: Char
hunterCell = '!'

targetCell :: Char
targetCell = '*'

gatewayCell :: Char
gatewayCell = '#'

obstacleCell :: Char
obstacleCell = '@'


data Game = Game {
            hunter :: Position,
            targets :: [Target],
            gateways :: [(Position, Position)],
            obstacles :: [Position]
            } deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString game = take ((length output) - 1) output
    where
        output = [symbol x y | x <- [0..n],  y <- [0..(m + 1)]]
            where
                symbol x y 
                    | y == (m + 1) && x /= n = '\n'
                    | length (targetPos x y) == 1 = targetCell 
                    | elem (x, y) (obstacles game) = obstacleCell
                    | elem (x, y) (fst (unzip (gateways game)))  || elem (x, y) (snd (unzip (gateways game))) = gatewayCell
                    | (x, y) == (hunter game) = hunterCell
                    | otherwise = blankCell

                size = last (obstacles game)
                n = fst size
                m = snd size
                targetPos x y = filter (\a -> (position a) == (x, y)) (targets game)


instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}

emptyGame :: Int -> Int -> Game
emptyGame n m = Game {hunter = (1, 1), targets = [], obstacles = obst, gateways = []}
    where
        obst = [(x, y) | x <- [0..(n-1)],  y <- [0..(m-1)], (x == 0 || x == (n - 1) || (y == 0 && (x /= 0 || x /= (n-1))) || (y == (m-1) && (x /= 0 || x /= (n-1))))]
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter pos game = newGame
    where
        size = last (obstacles game)
        n = fst size
        m = snd size
        targetPos = filter (\a -> (position a) == pos) (targets game)

        newGame
            | elem pos (obstacles game) = game
            | pos == (hunter game) = game
            | elem pos (fst (unzip (gateways game))) || elem pos (snd (unzip (gateways game))) = game
            | length targetPos == 1 = game
            | fst pos > n || fst pos < 0 || snd pos > m || snd pos < 0 = game  
            | otherwise = Game {hunter = pos, targets = (targets game), obstacles = (obstacles game), gateways = (gateways game)}

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget b pos game =  game {targets = (Target{position = pos, behavior = b} : (targets game))}

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (start, end)  game = game {gateways = (start, end) : (gateways game)}

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game = game {obstacles = (pos : (obstacles game))}

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game
    | elem pos (obstacles game) = Nothing
    | res /= Nothing = res
    | pos == (hunter game) = Nothing
    | length targetPos == 1 = Nothing
    | otherwise = Just pos
    where
        targetPos = filter (\a -> (position a) == pos) (targets game)
        first = filter (\x -> fst x == pos) (gateways game)
        second = filter (\x -> snd x == pos) (gateways game)
        res
            |length first == 1 = Just (snd (head first))
            |length second == 1 = Just (fst (head second))
            |otherwise =  Nothing

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

        
gatewayPair :: Position -> Game -> Position
gatewayPair pos game = result
    where
        first = filter (\x -> fst x == pos) (gateways game)
        second = filter (\x -> snd x == pos) (gateways game)
        result 
            | length first == 1 = snd (head first)
            | length second == 1 = fst (head second)
            | otherwise = pos

goEast :: Behavior
goEast pos game = Target {position = newPos, behavior = goEast}
    where
        gatePair = gatewayPair pos game  
        
        newPos = case (attemptMove (fst pos, snd pos + 1) game) of 
            Nothing -> gatePair
            Just val -> val
            
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos game = Target {position = newPos, behavior = goWest}
    where
        gatePair = gatewayPair pos game  
        
        newPos = case (attemptMove (fst pos, snd pos - 1) game) of 
            Nothing -> gatePair
            Just val -> val

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos game = Target {position = newPos, behavior = goNorth}
    where
        gatePair = gatewayPair pos game  
        
        newPos = case (attemptMove (fst pos - 1, snd pos) game) of 
            Nothing -> gatePair
            Just val -> val

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos game = Target {position = newPos, behavior = goSouth}
    where
        gatePair = gatewayPair pos game  
        
        newPos = case (attemptMove (fst pos + 1, snd pos) game) of 
            Nothing -> gatePair
            Just val -> val

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce param pos game = Target {position = newPos, behavior = newBehavior}
    where
        nT = (goNorth pos game)
        nP = (position nT)
        
        sT = (goSouth pos game)
        sP = (position sT)

        first = filter (\x -> fst x == pos) (gateways game)
        second = filter (\x -> snd x == pos) (gateways game)
        result 
            |length first == 1 = snd (head first)
            |length second == 1 = fst (head second)
            |otherwise = pos
        
        specialCase
            | param == 1 = case attemptMove ((fst pos) + 1, snd pos) game of
                Nothing -> pos
                Just value -> value
            |otherwise = case attemptMove ((fst pos) - 1, snd pos) game of
                Nothing -> pos
                Just value -> value
        
        newPos
            | param == 1 && specialCase == pos && sP == result  = nP
            | param == -1 && specialCase == pos && nP == result = sP
            | param == 1 = if pos == sP then nP else sP
            | otherwise = if pos == nP then sP else nP

        newBehavior
            | param == 1 && specialCase == pos && sP == result = bounce (-1)
            | param == -1 && specialCase == pos && nP == result = bounce 1
            | param == 1 = if pos == sP then bounce (-1) else bounce 1
            | otherwise = if pos == nP then bounce 1 else bounce (-1)

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game =  game {targets = moveTargetsHelper (targets game) game}

moveTargetsHelper :: [Target] -> Game -> [Target]
moveTargetsHelper l game = foldl (\acc (Target pos b) -> (b pos game) : acc) [] l

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled hunterPos (Target pos _) = result
    where
        result
            | (fst pos == fst hunterPos) && (snd pos == (snd hunterPos + 1) || snd pos == (snd hunterPos - 1)) = True
            | (snd pos == snd hunterPos) && (fst pos == (fst hunterPos + 1) || fst pos == (fst hunterPos - 1)) = True
            | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}



advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction kill game = game {hunter = newHunter, targets = newTargets}
    where
        nextPos = case direction of
            North -> (fst (hunter game) - 1, snd (hunter game))
            South -> (fst (hunter game) + 1, snd (hunter game))
            East -> (fst (hunter game), snd (hunter game) + 1)
            West -> (fst (hunter game), snd (hunter game) - 1)

        newHunter
            | elem nextPos (obstacles game) = hunter game
            | gatewayPair nextPos game == nextPos = nextPos
            | otherwise = gatewayPair nextPos game

        newTargets
            | kill == True = removeTargets newHunter (targets (moveTargets (game {targets = killTargets})))
            | otherwise = targets game
        
        killTargets = removeTargets newHunter (targets game) 

removeTargets :: Position -> [Target] -> [Target]
removeTargets hunterPos targetList = filter (\target -> isTargetKilled hunterPos target == False) targetList

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = not (null (targets game))

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game =  [northState, southState, eastState, westState]
        where
            northState = (North, advanceGameState North False game)
            southState = (South, advanceGameState South False game)
            eastState = (East, advanceGameState East False game)
            westState = (West, advanceGameState West False game)

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game = isTargetKilled (hunter game) (last (targets game))

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game = hEuclidean (hunter game) (position (last (targets game)))
{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int
    
hEuristic :: Position -> Game -> Float
hEuristic hunterPos game = minDist
    where
        minDist = minimum [(minimum gatewayFirst), (minimum gatewaySecond), dist]
        dist = hEuclidean hunterPos (position (last (targets game)))
        gatewayFirst = foldl (\acc ((x1,y1), (x2, y2)) -> ((hEuclidean hunterPos (x1, y1)) + (hEuclidean (x2, y2) (position (last (targets game))))) : acc) [] (gateways game)
        gatewaySecond = foldl (\acc ((x1,y1), (x2, y2)) -> ((hEuclidean hunterPos (x2, y2)) + (hEuclidean (x1, y1) (position (last (targets game))))) : acc) [] (gateways game)

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors (BonusGame game) =  [northState, southState, eastState, westState]
        where
            northState = (North, BonusGame (advanceGameState North False game))
            southState = (South, BonusGame (advanceGameState South False game))
            eastState = (East, BonusGame (advanceGameState East False game))
            westState = (West, BonusGame (advanceGameState West False game))

    {-`
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame game) = isTargetKilled (hunter game) (last (targets game))

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h (BonusGame game) = hEuristic (hunter game) game
