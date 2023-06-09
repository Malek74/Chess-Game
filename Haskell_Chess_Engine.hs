main :: IO ()
main = return ()

type Location = (Char, Int)

data Player = White | Black deriving (Show, Eq)

data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)

type Board = (Player, [Piece], [Piece])

-- sets the intial board config
setBoard :: Board
setBoard = (White, [R ('h', 1), N ('g', 1), B ('f', 1), K ('e', 1), Q ('d', 1), B ('c', 1), N ('b', 1), R ('a', 1), P ('h', 2), P ('g', 2), P ('f', 2), P ('e', 2), P ('d', 2), P ('c', 2), P ('b', 2), P ('a', 2)], [R ('h', 8), N ('g', 8), B ('f', 8), K ('e', 8), Q ('d', 8), B ('c', 8), N ('b', 8), R ('a', 8), P ('h', 7), P ('g', 7), P ('f', 7), P ('e', 7), P ('d', 7), P ('c', 7), P ('b', 7), P ('a', 7)])

visualizeBoard :: Board -> String
visualizeBoard (White, whitePieces, blackPieces) = "    a    b    c    d    e    f    g    h\n" ++ printBoard [8, 7, 6, 5, 4, 3, 2, 1] ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (whitePieces ++ blackPieces) whitePieces ++ "\nTurn: White"
visualizeBoard (Black, whitePieces, blackPieces) = "    a    b    c    d    e    f    g    h\n" ++ printBoard [8, 7, 6, 5, 4, 3, 2, 1] ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (whitePieces ++ blackPieces) whitePieces ++ "\nTurn: Black"

isLegal :: Piece -> Board -> Location -> Bool
isLegal (P location) (player, wp, bp) newLocation = if elem (P location) wp then newLocation `elem` getPawnLegal wp bp (P location) White else newLocation `elem` getPawnLegal wp bp (P location) Black
isLegal (R (c, r)) (player, wp, bp) newLocation = if elem (R (c, r)) wp then newLocation `elem` validMovmentsRook c r (c, r) wp bp White else newLocation `elem` validMovmentsRook c r (c, r) wp bp Black
isLegal (B location) (player, whitePieces, blackPieces) newLocation = elem newLocation (getBishopLegal whitePieces blackPieces (B location) White) || elem newLocation (getBishopLegal whitePieces blackPieces (B location) Black)
isLegal (K location) (player, whitePieces, blackPieces) newLocation = newLocation `elem` validMovesKing (allMovesKing (K location)) player whitePieces blackPieces
isLegal (Q (c, r)) (player, wp, bp) newLocation = elem newLocation (legalMovesQueen wp bp (Q (c, r)) White)
isLegal (N (c, r)) (player, wp, bp) newLocation = if elem (N (c, r)) wp then newLocation `elem` suggestMoveKnight (N (c, r)) (White, wp, bp) else newLocation `elem` suggestMoveKnight (N (c, r)) (Black, wp, bp)

suggestMove :: Piece -> Board -> [Location]
suggestMove (P (letter, row)) (player, whitePieces, blackPieces) = if elem ((P (letter, row))) whitePieces then getPawnLegal whitePieces blackPieces (P (letter, row)) White else getPawnLegal whitePieces blackPieces (P (letter, row)) Black
suggestMove (B (letter, row)) (player, whitePieces, blackPieces) = if elem ((B (letter, row))) whitePieces then getBishopLegal whitePieces blackPieces (B (letter, row)) White else getBishopLegal whitePieces blackPieces (B (letter, row)) Black
suggestMove (R (letter, row)) (player, whitePieces, blackPieces) = if elem ((R (letter, row))) whitePieces then validMovmentsRook letter row (letter, row) whitePieces blackPieces White else validMovmentsRook letter row (letter, row) whitePieces blackPieces Black
suggestMove (K (letter, row)) (player, whitePieces, blackPieces) = if elem ((K (letter, row))) whitePieces then validMovesKing (allMovesKing (K (letter, row))) White whitePieces blackPieces else validMovesKing (allMovesKing (K (letter, row))) Black whitePieces blackPieces
suggestMove (Q (letter, row)) (player, whitePieces, blackPieces) = if elem ((Q (letter, row))) whitePieces then legalMovesQueen whitePieces blackPieces (Q (letter, row)) White else legalMovesQueen whitePieces blackPieces (Q (letter, row)) Black
suggestMove (N (letter, row)) (player, whitePieces, blackPieces) = if elem ((N (letter, row))) whitePieces then suggestMoveKnight (N (letter, row)) (White, whitePieces, blackPieces) else suggestMoveKnight (N (letter, row)) (Black, whitePieces, blackPieces)

move :: Piece -> Location -> Board -> Board
move piece newlocation (player, wp, bp)
  | player == White && not (elem piece wp) = error "This is White player's turn, Black can't move."
  | player == Black && not (elem piece bp) = error "This is Black player's turn, White can't move."
  | not (isLegal piece (player, wp, bp) newlocation) = error ("Illegal move for pieace" ++ (show piece))
  | player == White = (Black, (replace piece (setPieceLocation piece newlocation) wp), (removal newlocation bp))
  | otherwise = (White, (removal newlocation wp), (replace piece (setPieceLocation piece newlocation) bp))

---HELPER METHODS--

-- helper that gets all possible movement of knight
allMovesKnight :: Piece -> [Location]
allMovesKnight (N (s, n)) =
  insideBounds
    ( [(succ (succ s), n + 1)]
        ++ [(succ (succ s), n - 1)]
        ++ [(pred (pred s), n + 1)]
        ++ [(pred (pred s), n - 1)]
        ++ [(succ s, n + 2)]
        ++ [(succ s, n - 2)]
        ++ [(pred s, n + 2)]
        ++ [(pred s, n - 2)]
    )

-- gets valid movements of knight
suggestMoveKnight :: Piece -> Board -> [Location]
suggestMoveKnight (N (s, n)) (player, whitePieces, blackPieces)
  | player == White = filterPieces whitePieces (allMovesKnight (N (s, n)))
  | otherwise = filterPieces blackPieces (allMovesKnight (N (s, n)))

-- filters valid locations that knight can move to
filterPieces :: [Piece] -> [Location] -> [Location]
filterPieces [] locations = locations
filterPieces ((P (r, l)) : t) locations = filter (\(row, letter) -> not (row == r && letter == l)) (filterPieces t locations)
filterPieces ((N (r, l)) : t) locations = filter (\(row, letter) -> not (row == r && letter == l)) (filterPieces t locations)
filterPieces ((K (r, l)) : t) locations = filter (\(row, letter) -> not (row == r && letter == l)) (filterPieces t locations)
filterPieces ((Q (r, l)) : t) locations = filter (\(row, letter) -> not (row == r && letter == l)) (filterPieces t locations)
filterPieces ((R (r, l)) : t) locations = filter (\(row, letter) -> not (row == r && letter == l)) (filterPieces t locations)
filterPieces ((B (r, l)) : t) locations = filter (\(row, letter) -> not (row == r && letter == l)) (filterPieces t locations)

-- Helper to get Pawn legal moves
getPawnLegal :: [Piece] -> [Piece] -> Piece -> Player -> [Location]
getPawnLegal wPieces bPieces (P (letter, row)) player
  | player == White && row == 2 && not (checkPieceAtLocation wPieces (letter, 3)) && not (checkPieceAtLocation bPieces (letter, 3)) && not (checkPieceAtLocation wPieces (letter, 4)) && not (checkPieceAtLocation bPieces (letter, 4)) = [(letter, 3), (letter, 4)] ++ pawnDiagonal wPieces bPieces (letter, row) player
  | player == White && row == 2 && not (checkPieceAtLocation wPieces (letter, 3)) && not (checkPieceAtLocation bPieces (letter, 3)) = (letter, 3) : pawnDiagonal wPieces bPieces (letter, row) player
  | player == Black && row == 7 && not (checkPieceAtLocation wPieces (letter, 6)) && not (checkPieceAtLocation bPieces (letter, 6)) && not (checkPieceAtLocation wPieces (letter, 5)) && not (checkPieceAtLocation bPieces (letter, 5)) = [(letter, 6), (letter, 5)] ++ pawnDiagonal wPieces bPieces (letter, row) player
  | player == Black && row == 7 && not (checkPieceAtLocation wPieces (letter, 6)) && not (checkPieceAtLocation bPieces (letter, 6)) = (letter, 6) : pawnDiagonal wPieces bPieces (letter, row) player
  | player == White && row < 8 && not (checkPieceAtLocation wPieces (letter, row + 1)) && not (checkPieceAtLocation bPieces (letter, row + 1)) = (letter, row + 1) : pawnDiagonal wPieces bPieces (letter, row) player
  | player == Black && row > 1 && not (checkPieceAtLocation wPieces (letter, row - 1)) && not (checkPieceAtLocation bPieces (letter, row - 1)) = (letter, row - 1) : pawnDiagonal wPieces bPieces (letter, row) player
  | otherwise = pawnDiagonal wPieces bPieces (letter, row) player

-- helper to check if there is a piece in given location
checkPieceAtLocation :: [Piece] -> Location -> Bool
checkPieceAtLocation [] (_, _) = False
checkPieceAtLocation ((P (r, l)) : t) (row, letter)
  | row == r && letter == l = True
  | otherwise = checkPieceAtLocation t (row, letter)
checkPieceAtLocation ((N (r, l)) : t) (row, letter)
  | row == r && letter == l = True
  | otherwise = checkPieceAtLocation t (row, letter)
checkPieceAtLocation ((K (r, l)) : t) (row, letter)
  | row == r && letter == l = True
  | otherwise = checkPieceAtLocation t (row, letter)
checkPieceAtLocation ((Q (r, l)) : t) (row, letter)
  | row == r && letter == l = True
  | otherwise = checkPieceAtLocation t (row, letter)
checkPieceAtLocation ((R (r, l)) : t) (row, letter)
  | row == r && letter == l = True
  | otherwise = checkPieceAtLocation t (row, letter)
checkPieceAtLocation ((B (r, l)) : t) (row, letter)
  | row == r && letter == l = True
  | otherwise = checkPieceAtLocation t (row, letter)

-- helper to get pawn's diagonals
pawnDiagonal :: [Piece] -> [Piece] -> Location -> Player -> [Location]
pawnDiagonal wPieces bPieces (letter, row) player
  | letter == 'a' && player == White && checkPieceAtLocation bPieces ('b', row + 1) = [('b', row + 1)]
  | letter == 'a' && player == Black && checkPieceAtLocation wPieces ('b', row - 1) = [('b', row - 1)]
  | letter == 'h' && player == White && checkPieceAtLocation bPieces ('g', row + 1) = [('g', row + 1)]
  | letter == 'h' && player == Black && checkPieceAtLocation wPieces ('g', row - 1) = [('g', row - 1)]
  | player == White = getPawnDiagonal bPieces (pred letter, row + 1) ++ getPawnDiagonal bPieces (succ letter, row + 1)
  | player == Black = getPawnDiagonal wPieces (pred letter, row - 1) ++ getPawnDiagonal wPieces (succ letter, row - 1)

getPawnDiagonal :: [Piece] -> Location -> [Location]
getPawnDiagonal pieces (letter, row)
  | checkPieceAtLocation pieces (letter, row) = [(letter, row)]
  | otherwise = []

-- helper to get bishop legal moves
getBishopLegal :: [Piece] -> [Piece] -> Piece -> Player -> [Location]
getBishopLegal wPieces bPieces (B (letter, row)) player
  | player == White = upperLeftDiagonal wPieces bPieces (letter, row) ++ upperRightDiagonal wPieces bPieces (letter, row) ++ lowerLeftDiagonal wPieces bPieces (letter, row) ++ lowerRightDiagonal wPieces bPieces (letter, row)
  | player == Black = upperLeftDiagonal bPieces wPieces (letter, row) ++ upperRightDiagonal bPieces wPieces (letter, row) ++ lowerLeftDiagonal bPieces wPieces (letter, row) ++ lowerRightDiagonal bPieces wPieces (letter, row)
  | otherwise = []

-- helper to get bishop's diagonal
upperLeftDiagonal :: [Piece] -> [Piece] -> Location -> [Location]
upperLeftDiagonal friends enemies (letter, row)
  | pred letter == '`' || row + 1 == 9 = []
  | pred letter == 'a' && not (checkPieceAtLocation friends ('a', row + 1)) = [('a', row + 1)]
  | not (checkPieceAtLocation friends ((pred letter), row + 1)) && not (checkPieceAtLocation enemies ((pred letter), row + 1)) = ((pred letter), row + 1) : upperLeftDiagonal friends enemies ((pred letter), row + 1)
  | checkPieceAtLocation enemies ((pred letter), row + 1) = [(pred letter, row + 1)]
  | otherwise = []

upperRightDiagonal :: [Piece] -> [Piece] -> Location -> [Location]
upperRightDiagonal friends enemies (letter, row)
  | succ letter == 'i' || row + 1 == 9 = []
  | succ letter == 'h' && not (checkPieceAtLocation friends ('h', row + 1)) = [('h', row + 1)]
  | not (checkPieceAtLocation friends ((succ letter), row + 1)) && not (checkPieceAtLocation enemies ((succ letter), row + 1)) = ((succ letter), row + 1) : upperRightDiagonal friends enemies ((succ letter), row + 1)
  | checkPieceAtLocation enemies ((succ letter), row + 1) = [(succ letter, row + 1)]
  | otherwise = []

lowerLeftDiagonal :: [Piece] -> [Piece] -> Location -> [Location]
lowerLeftDiagonal friends enemies (letter, row)
  | pred letter == '`' || row - 1 == 0 = []
  | pred letter == 'a' && not (checkPieceAtLocation friends ('a', row - 1)) = [('a', row - 1)]
  | not (checkPieceAtLocation friends ((pred letter), row - 1)) && not (checkPieceAtLocation enemies ((pred letter), row - 1)) = ((pred letter), row - 1) : lowerLeftDiagonal friends enemies ((pred letter), row - 1)
  | checkPieceAtLocation enemies ((pred letter), row - 1) = [(pred letter, row - 1)]
  | otherwise = []

lowerRightDiagonal :: [Piece] -> [Piece] -> Location -> [Location]
lowerRightDiagonal friends enemies (letter, row)
  | succ letter == 'i' || row - 1 == 0 = []
  | succ letter == 'h' && not (checkPieceAtLocation friends ('h', row - 1)) = [('h', row - 1)]
  | not (checkPieceAtLocation friends ((succ letter), row - 1)) && not (checkPieceAtLocation enemies ((succ letter), row - 1)) = ((succ letter), row - 1) : lowerRightDiagonal friends enemies ((succ letter), row - 1)
  | checkPieceAtLocation enemies ((succ letter), row - 1) = [(succ letter, row - 1)]
  | otherwise = []

-- helper to get valid Rook Movement
validMovmentsRook :: Char -> Int -> Location -> [Piece] -> [Piece] -> Player -> [Location]
validMovmentsRook col row (c, r) wPieces bPieces player = insideBounds (getLeftMovementRook (pred col) (pred col, r) wPieces bPieces player ++ getRightMovementRook (succ col) (succ col, r) wPieces bPieces player ++ getUpperMovementRook (row + 1) (c, r + 1) wPieces bPieces player ++ getLowerMovementRook (row - 1) (c, r - 1) wPieces bPieces player)

-- helper to get Rook's directions
getRightMovementRook :: Char -> Location -> [Piece] -> [Piece] -> Player -> [Location]
getRightMovementRook 'i' _ _ _ _ = []
getRightMovementRook col (c, r) wPieces bPieces player =
  if checkPieceAtLocation (wPieces ++ bPieces) (c, r)
    then (if (checkPieceAtLocation wPieces (c, r) && player == Black) || (checkPieceAtLocation bPieces (c, r) && player == White) then [(c, r)] else [])
    else (c, r) : getRightMovementRook (succ (col)) (succ (col), r) wPieces bPieces player

getLeftMovementRook :: Char -> Location -> [Piece] -> [Piece] -> Player -> [Location]
getLeftMovementRook '`' _ _ _ _ = []
getLeftMovementRook col (c, r) wPieces bPieces player =
  if checkPieceAtLocation (wPieces ++ bPieces) (c, r)
    then (if (checkPieceAtLocation wPieces (c, r) && player == Black) || (checkPieceAtLocation bPieces (c, r) && player == White) then [(c, r)] else [])
    else (c, r) : getLeftMovementRook (pred (col)) (pred (col), r) wPieces bPieces player

getUpperMovementRook :: Int -> Location -> [Piece] -> [Piece] -> Player -> [Location]
getUpperMovementRook 9 _ _ _ _ = []
getUpperMovementRook row (c, r) wPieces bPieces player =
  if checkPieceAtLocation (wPieces ++ bPieces) (c, r)
    then (if (checkPieceAtLocation wPieces (c, r) && player == Black) || (checkPieceAtLocation bPieces (c, r) && player == White) then [(c, row)] else [])
    else (c, row) : getUpperMovementRook (row + 1) (c, row + 1) wPieces bPieces player

getLowerMovementRook :: Int -> Location -> [Piece] -> [Piece] -> Player -> [Location]
getLowerMovementRook (-1) _ _ _ _ = []
getLowerMovementRook row (c, r) wPieces bPieces player =
  if checkPieceAtLocation (wPieces ++ bPieces) (c, r)
    then (if (checkPieceAtLocation wPieces (c, r) && player == Black) || (checkPieceAtLocation bPieces (c, r) && player == White) then [(c, row)] else [])
    else (c, row) : getLowerMovementRook (row - 1) (c, row - 1) wPieces bPieces player

-- helper to get all valid movements of king
validMovesKing :: [Location] -> Player -> [Piece] -> [Piece] -> [Location]
validMovesKing [] _ _ _ = []
validMovesKing (h : t) player wPieces bPieces
  | player == White && checkPieceAtLocation bPieces h = [h] ++ validMovesKing t player wPieces bPieces
  | player == Black && checkPieceAtLocation wPieces h = [h] ++ validMovesKing t player wPieces bPieces
  | not (checkPieceAtLocation wPieces h) && not (checkPieceAtLocation bPieces h) = [h] ++ validMovesKing t player wPieces bPieces
  | otherwise = validMovesKing t player wPieces bPieces

allMovesKing :: Piece -> [Location]
allMovesKing (K (s, n)) =
  insideBounds
    ( [(s, n + 1)]
        ++ [(s, n - 1)]
        ++ [(pred s, n - 1)]
        ++ [(pred s, n + 1)]
        ++ [(succ s, n - 1)]
        ++ [(succ s, n + 1)]
        ++ [(succ s, n)]
        ++ [(pred s, n)]
    )

legalMovesQueen whitePieces blackPieces (Q (letter, row)) player = getBishopLegal whitePieces blackPieces (B (letter, row)) player ++ validMovmentsRook letter row (letter, row) whitePieces blackPieces player

-- helper checks that all locations are within board
insideBounds :: [Location] -> [Location]
insideBounds = filter (\(c, i) -> elem c ['a' .. 'h'] && elem i [1 .. 8])

-- visualizeBoard Helpers
printBoard :: [Int] -> String -> [Piece] -> [Piece] -> String
printBoard [] _ _ _ = ""
printBoard (h : t) cols pieces wp = concatIntString h (printRow h cols pieces wp) ++ "\n" ++ printBoard t cols pieces wp

printRow :: Int -> String -> [Piece] -> [Piece] -> String
printRow rowNum [] _ _ = " |"
printRow rowNum (h : t) pieces wp = " | " ++ helper rowNum h pieces wp ++ printRow rowNum t pieces wp

helper :: Int -> Char -> [Piece] -> [Piece] -> String
helper row col [] _ = "  " -- Base case: empty list, no piece found
helper row col (N (ypos, xpos) : t) whitePieces
  | row == xpos && ypos == col = if N (ypos, xpos) `elem` whitePieces then printPiece White (N (ypos, xpos)) else printPiece Black (N (ypos, xpos))
  | otherwise = helper row col t whitePieces
helper row col (P (ypos, xpos) : t) whitePieces
  | row == xpos && ypos == col = if P (ypos, xpos) `elem` whitePieces then printPiece White (P (ypos, xpos)) else printPiece Black (P (ypos, xpos))
  | otherwise = helper row col t whitePieces
helper row col (K (ypos, xpos) : t) whitePieces
  | row == xpos && ypos == col = if K (ypos, xpos) `elem` whitePieces then printPiece White (K (ypos, xpos)) else printPiece Black (K (ypos, xpos))
  | otherwise = helper row col t whitePieces
helper row col (Q (ypos, xpos) : t) whitePieces
  | row == xpos && ypos == col = if Q (ypos, xpos) `elem` whitePieces then printPiece White (Q (ypos, xpos)) else printPiece Black (Q (ypos, xpos))
  | otherwise = helper row col t whitePieces
helper row col (R (ypos, xpos) : t) whitePieces
  | row == xpos && ypos == col = if R (ypos, xpos) `elem` whitePieces then printPiece White (R (ypos, xpos)) else printPiece Black (R (ypos, xpos))
  | otherwise = helper row col t whitePieces
helper row col (B (ypos, xpos) : t) whitePieces
  | row == xpos && ypos == col = if B (ypos, xpos) `elem` whitePieces then printPiece White (B (ypos, xpos)) else printPiece Black (B (ypos, xpos))
  | otherwise = helper row col t whitePieces

printPiece :: Player -> Piece -> String
printPiece player (P _) = if player == White then "PW" else "PB"
printPiece player (N _) = if player == White then "NW" else "NB"
printPiece player (K _) = if player == White then "KW" else "KB"
printPiece player (Q _) = if player == White then "QW" else "QB"
printPiece player (R _) = if player == White then "RW" else "RB"
printPiece player (B _) = if player == White then "BW" else "BB"

concatIntString :: Int -> String -> String
concatIntString n str = show n ++ str

-- helper replaces an element in a list
replace _ _ [] = []
replace old new (h : t)
  | h == old = new : replace old new t
  | otherwise = h : replace old new t

removal :: Location -> [Piece] -> [Piece]
removal _ [] = []
removal (loc) ((P (loc2)) : t)
  | loc == loc2 = t
  | otherwise = [(P (loc2))] ++ removal loc t
removal (loc) ((B (loc2)) : t)
  | loc == loc2 = t
  | otherwise = [(B (loc2))] ++ removal loc t
removal (loc) ((N (loc2)) : t)
  | loc == loc2 = t
  | otherwise = [(N (loc2))] ++ removal loc t
removal (loc) ((R (loc2)) : t)
  | loc == loc2 = t
  | otherwise = [(R (loc2))] ++ removal loc t
removal (loc) ((K (loc2)) : t)
  | loc == loc2 = t
  | otherwise = [(K (loc2))] ++ removal loc t
removal (loc) ((Q (loc2)) : t)
  | loc == loc2 = t
  | otherwise = [(Q (loc2))] ++ removal loc t

-- Helper updates piece location
setPieceLocation :: Piece -> Location -> Piece
setPieceLocation (N location) (c, r) = N (c, r)
setPieceLocation (R location) (c, r) = R (c, r)
setPieceLocation (P location) (c, r) = P (c, r)
setPieceLocation (K location) (c, r) = K (c, r)
setPieceLocation (Q location) (c, r) = Q (c, r)
setPieceLocation (B location) (c, r) = B (c, r)
