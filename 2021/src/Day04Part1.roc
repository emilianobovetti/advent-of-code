app "day04-part1"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.Stdout, base.File.{ Handle } ]
    provides [ main ] to base


AppError : [ InvalidNum Str, NoWinnerFound ]

BoardLine : List { num: U64, marked: Bool }

Board : List BoardLine


parseU64Loop : List U8, U64 -> Result U64 {}
parseU64Loop = \bytes, acc ->
    { before, others: tl } = List.split bytes 1

    when List.first before is
        Err ListWasEmpty ->
            Ok acc

        Ok char if char >= 48 && char <= 57 ->
            parseU64Loop tl (acc * 10 + Num.intCast char - 48)

        Ok _ ->
            Err {}


parseU64 : Str -> Result U64 AppError
parseU64 = \str ->
    str
    |> Str.toUtf8
    |> parseU64Loop 0
    |> Result.mapErr (\{} -> InvalidNum str)


resultPartition : List (Result ok err) -> { ok: List ok, err: List err }
resultPartition = \list ->
    { ok, err }, elem <- List.walk list { ok: [], err: [] }

    when elem is
        Ok val -> { ok: List.append ok val, err }
        Err e -> { ok, err: List.append err e }


# It looks like this should be in standard library (List.mapOrCancel)
mapOrCancel : List before, (before -> Result after err) -> Result (List after) err
mapOrCancel = \list, fn ->
    { ok, err } = resultPartition (List.map list fn)

    when List.first err is
        Err ListWasEmpty -> Ok ok
        Ok firstErr -> Err firstErr


# It looks like this should be in standard library (List.mapOrDrop)
mapOrDrop : List before, (before -> [ Keep after, Drop ]) -> List after
mapOrDrop = \list, fn ->
    acc, elem <- List.walk list []

    when fn elem is
        Keep val -> List.append acc val
        Drop -> acc


trimAndDrop : Str -> [ Keep Str, Drop ]
trimAndDrop = \str ->
    when Str.trim str is
        "" -> Drop
        s -> Keep s


parseU64List : Str, Str -> Result (List U64) AppError
parseU64List = \str, separator ->
    str
    |> Str.split separator
    |> mapOrDrop trimAndDrop
    |> mapOrCancel parseU64


readLine : Handle, (Str -> Task ok err) -> Task ok err
readLine = \handle, fn ->
    lineWithNL <- Task.await (File.line handle)

    fn (Str.trim lineWithNL)


readLinesLoop : Handle, List Str -> Task (List Str) *
readLinesLoop = \handle, buffer ->
    line <- readLine handle

    when List.last buffer is
        Ok "" if line == "" -> Task.succeed buffer
        _ -> readLinesLoop handle (List.append buffer line)


## Reads all lines until it finds two consecutive "\n".
## Ideally this could read lines until it finds and EOF,
## but I can't figure out how 🤷.
readLines : Handle, (List Str -> Task ok err) -> Task ok err
readLines = \handle, fn ->
    lines <- Task.await (readLinesLoop handle [])

    fn lines


readDrawnNumbers : Handle -> Task (List U64) AppError
readDrawnNumbers = \handle ->
    firstLine <- readLine handle

    Task.fromResult (parseU64List firstLine ",")


parseBoard : List Str -> Result Board AppError
parseBoard = \lines ->
    line <- mapOrCancel lines
    nums <- Result.map (parseU64List line " ")
    num <- List.map nums

    { num, marked: False }


chunkLoop : List a, Nat -> { chunks: List (List a), buffer: List a }
chunkLoop = \list, size ->
    acc, elem <- List.walk list { chunks: [], buffer: [] }

    buffer = List.append acc.buffer elem

    if List.len buffer < size then
        { acc & buffer }
    else
        { chunks: List.append acc.chunks buffer, buffer: [] }


chunk : List a, Nat -> List (List a)
chunk = \list, size ->
    { chunks, buffer } = chunkLoop list size

    # Ideally I'd put a `List.isEmpty buffer` here,
    # but for some reason it throws a
    #
    #   Compiler bug: argument #1st to low-level operation Eq was the wrong type!
    #
    if buffer == [] then
        chunks
    else
        List.append chunks buffer


readBoards : Handle -> Task (List Board) AppError
readBoards = \handle ->
    lines <- readLines handle

    lines
    |> mapOrDrop trimAndDrop
    |> chunk 5
    |> mapOrCancel parseBoard
    |> Task.fromResult


errorToString : AppError -> Str
errorToString = \err ->
    when err is
        InvalidNum str -> "Invalid number found: \(str)"
        NoWinnerFound -> "Apparently there's no winner :("


boardToStr : Board -> Str
boardToStr = \board ->
    acc, line <- List.walkBackwards board ""

    line
    |> List.map (\{ num } -> Num.toStr num)
    |> Str.joinWith ","
    |> Str.concat "\n"
    |> Str.concat acc


printBoard : Board -> Task {} *
printBoard = \board ->
    Stdout.line (boardToStr board)


printBoards : List Board -> Task {} *
printBoards = \boards ->
    { before, others: tl } = List.split boards 1

    when List.first before is
        Err ListWasEmpty -> Task.succeed {}
        Ok board -> Task.await (printBoard board) \{} -> printBoards tl


markBoard : Board, U64 -> Board
markBoard = \board, drawnNum ->
    line <- List.map board
    item <- List.map line

    if item.num == drawnNum then
        { item & marked: True }
    else
        item


markBoards : List Board, U64 -> List Board
markBoards = \boards, drawnNum ->
    board <- List.map boards

    markBoard board drawnNum


allNumsMarked : BoardLine -> Bool
allNumsMarked = \row ->
    List.all row \{ marked } -> marked


hasWinningRow : Board -> Bool
hasWinningRow = \board ->
    List.any board allNumsMarked


getColumn : Board, Nat -> BoardLine
getColumn = \board, index ->
    row <- List.map board

    row
    |> List.get index
    |> Result.withDefault { num: -1, marked: False }
    # Let's make a phony item just because


hasWinningColumn : Board -> Bool
hasWinningColumn = \board ->
    numCols = List.first board |> Result.withDefault [] |> List.len
    index <- List.any (List.range 0 numCols)

    allNumsMarked (getColumn board index)


winningBoard : Board -> Bool
winningBoard = \board ->
    hasWinningRow board || hasWinningColumn board


sumUnmarkedNums : Board -> U64
sumUnmarkedNums = \board ->
    total, row <- List.walk board 0
    sum, { num, marked } <- List.walk row total

    if marked then sum else sum + num


## It looks like `List.find` and the implementation I left in
## DevUtils cause a segmentation fault on some dark edge cases.
find : List elem, (elem -> Bool) -> Result elem [ NotFound ]*
find = \list, pred ->
    { before, others: tl } = List.split list 1

    when List.first before is
        Err ListWasEmpty -> Err NotFound
        Ok hd if pred hd -> Ok hd
        Ok _ -> find tl pred


WinnerSearch : {
        boards: List Board,
        winner: Result { board: Board, lastDrawnNum: U64 } AppError,
    }

findWinner : List Board, List U64 -> WinnerSearch
findWinner = \startBoards, drawnNums ->
    acc, num <- List.walkUntil drawnNums { boards: startBoards, winner: Err NoWinnerFound }

    boards = markBoards acc.boards num

    when find boards winningBoard is
        Err NotFound -> Continue { acc & boards }
        Ok win -> Stop { boards, winner: Ok { board: win, lastDrawnNum: num } }


runGame : Handle -> Task {} AppError
runGame = \handle ->
    drawnNums <- Task.await (readDrawnNumbers handle)
    boards <- Task.await (readBoards handle)

    { winner } = findWinner boards drawnNums

    { board, lastDrawnNum } <- Task.await (Task.fromResult winner)

    sumUnmarked = sumUnmarkedNums board
    score = Num.toStr (sumUnmarked * lastDrawnNum)
    sum = Num.toStr sumUnmarked
    num = Num.toStr lastDrawnNum

    {} <- Task.await (Stdout.line "Winning board")
    {} <- Task.await (printBoard board)
    {} <- Task.await (Stdout.line "Sum of all unmarked numbers: \(sum)")
    {} <- Task.await (Stdout.line "Last drawn number: \(num)")

    Stdout.line "Score: \(score)"


main : Task {} *
main =
    handle <- File.withOpen "./data/day04-bingo"
    err <- Task.onFail (runGame handle)
    {} <- err |> errorToString |> Stdout.line |> Task.await

    # It seems that this little fella causes a segmentation fault
    #{} <- Task.await (File.close handle)

    Task.succeed {}
