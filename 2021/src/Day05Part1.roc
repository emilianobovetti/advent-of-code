app "day05-part1"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.Stdout, base.File.{ Handle } ]
    provides [ main ] to base


Assoc key val : List { key: key, val: val }

SearchResult elem err : Result elem [ NotFound ]err


find : List elem, (elem -> Bool) -> SearchResult elem *
find = \list, pred ->
    { before, others: tl } = List.split list 1

    when List.first before is
        Err ListWasEmpty -> Err NotFound
        Ok hd if pred hd -> Ok hd
        Ok _ -> find tl pred


addAssoc : Assoc key val, key, val -> Assoc key val
addAssoc = \assoc, key, val ->
    List.prepend assoc { key, val }


putAssoc : Assoc key val, key, val -> Assoc key val
putAssoc = \assoc, key, val ->
    assoc
    |> List.keepIf (\item -> item.key != key)
    |> addAssoc key val


findAssoc : Assoc key val, key -> SearchResult val *
findAssoc = \assoc, key ->
    assoc
    |> find (\item -> item.key == key)
    |> Result.map (\item -> item.val)


AppError : [ InvalidNumStr Str, InvalidVentLine Str ]

Coords : { x: Nat, y: Nat }

# It looks like that a `Dict` right now doesn't
# work well tags, records or strings,
# so we are using a `List` instead
WeightedGrid : Assoc Coords Nat


emptyGrid : WeightedGrid
emptyGrid = []


getValue : WeightedGrid, Coords -> Nat
getValue = \grid, coords ->
    findAssoc grid coords |> Result.withDefault 0


incValue : WeightedGrid, Coords -> WeightedGrid
incValue = \grid, coords ->
    putAssoc grid coords (getValue grid coords + 1)


readLine : Handle, (Str -> Task ok err) -> Task ok err
readLine = \handle, fn ->
    lineWithNL <- Task.await (File.line handle)

    fn (Str.trim lineWithNL)


readLinesLoop : Handle, Str, List Str -> Task (List Str) *
readLinesLoop = \handle, last, buffer ->
    current <- readLine handle

    when Pair last current is
        Pair "" "" -> Task.succeed buffer
        Pair "" _ -> readLinesLoop handle current buffer
        Pair _ _ -> readLinesLoop handle current (List.append buffer last)


readNonEmptyLines : Handle, (List Str -> Task ok err) -> Task ok err
readNonEmptyLines = \handle, fn ->
    first <- readLine handle

    Task.await (readLinesLoop handle first []) fn


print : Str, ({} -> Task ok err) -> Task ok err
print = \str, fn ->
    Task.await (Stdout.line str) fn


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


Vent : [
        Horizontal { y: Nat, x1: Nat, x2: Nat },
        Vertical { x: Nat, y1: Nat, y2: Nat },
    ]


ventParts : Str -> List Str
ventParts = \str ->
    Str.split str " -> "


coordParts : Str -> List Str
coordParts = \str ->
    Str.split str ","


getOrElse : List ok, Nat, err -> Result ok err
getOrElse = \list, index, err ->
    List.get list index |> Result.mapErr (\OutOfBounds -> err)


parseVent : Str -> Result Vent AppError
parseVent = \str ->
    parts : List Nat
    parts =
        ventParts str
        |> List.map coordParts
        |> List.join
        |> mapOrCancel Str.toNat
        |> Result.withDefault []

    getPart : Nat, (Nat -> Result Vent AppError) -> Result Vent AppError
    getPart = \index, fn ->
        getOrElse parts index (InvalidVentLine str) |> Result.after fn

    #x1 <- getPart 0
    #y1 <- getPart 1
    #x2 <- getPart 2
    #y2 <- getPart 3

    invalidLine = InvalidVentLine str

    #one <- getOrElse parts 0 invalidLine |> Result.after
    #two <- getOrElse parts 1 invalidLine |> Result.after
    #three <- getOrElse parts 2 invalidLine |> Result.after
    #four <- getOrElse parts 3 invalidLine |> Result.after

    Ok (Horizontal { y: 0, x1: 0, x2: 0 })


parseVents : List Str -> Result (List Vent) AppError
parseVents = \list ->
    mapOrCancel list parseVent


awaitResult : Result a err, (a -> Task b err) -> Task b err
awaitResult = \res, fn ->
    Task.fromResult res |> Task.await fn


mainTask : Handle -> Task {} AppError
mainTask = \handle ->
#    grid =
#        emptyGrid
#        |> putAssoc { x: 1, y: 1 } 2
#        |> incValue { x: 1, y: 1 }
#
#    res = getValue grid { x: 1, y: 1 } |> Num.toStr
#
#    {} <- Task.await (Stdout.line "Result: \(res)")

    lines <- readNonEmptyLines handle
    vents <- awaitResult (parseVents lines)

    {} <- print (Str.joinWith lines "\n")

    Stdout.line " === END === "


errorToStr : AppError -> Str
errorToStr = \err ->
    when err is
        InvalidNumStr str -> "Invalid number found: \(str)"
        InvalidVentLine str -> "Invalid vent found: \(str)"


main : Task {} *
main =
    handle <- File.withOpen "./data/day05-vents"
    err <- Task.onFail (mainTask handle)
    {} <- errorToStr err |> Stdout.line |> Task.await

    Task.succeed {}
