app "dev-utils"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.Stdout, base.File.{ Handle } ]
    provides [ main ] to base

# # # # # # # # #
#   Day03Part2  #
# # # # # # # # #

Bit : [ Zero, One ]

BitString : List Bit

bitToStr : Bit -> Str
bitToStr = \bit ->
    when bit is
        Zero -> "0"
        One -> "1"

## Segmentation fault (core dumped)
#bitsToStr : BitString -> Str
#bitsToStr = \bits ->
#    bits |> List.map bitToStr |> Str.joinWith ""

bitsToStr : BitString -> Str
bitsToStr = \bits ->
    acc, bit <- List.walk bits ""

    Str.concat acc (bitToStr bit)

## Segmentation fault (core dumped)
#printLines : List BitString -> Task {} *
#printLines = \lines ->
#    List.walk lines (Task.succeed {}) \task, line ->
#        {} <- Task.await task
#
#        Stdout.line (bitsToStr line)

printLines : List BitString -> Task {} *
printLines = \lines ->
    { before, others: tl } = List.split lines 1

    when List.first before is
        Err ListWasEmpty ->
            Task.succeed {}

        Ok hd ->
            {} <- hd |> bitsToStr |> Stdout.line |> Task.await

            printLines tl

# # # # # # # # #
#   Day04Part1  #
# # # # # # # # #

resultToStr : Result ok AppError, (ok -> Str) -> Str
resultToStr = \res, toStr ->
    when res is
        Ok val -> toStr val
        Err e -> errorToString e

printResult : Result ok AppError, (ok -> Str) -> Task {} *
printResult = \res, toStr ->
    Stdout.line (resultToStr res toStr)

awaitResult : Result a err, (a -> Task b err) -> Task b err
awaitResult = \res, fn ->
    Task.fromResult res |> Task.await fn

find : List elem, (elem -> Bool) -> Result elem [ NotFound ]*
find = \list, pred ->
    acc, elem <- List.walkUntil list (Err NotFound)

    if pred elem then
        Stop (Ok elem)
    else
        Continue acc

# $ roc repl
# » Ok []
# thread 'main' panicked at 'internal error: entered unreachable code: Something had a Struct layout, but instead of a Record type, it had: Structure(TagUnion(UnionTags { length: 1, tag_names_start: 37, variables_start: 36 }, 102))', cli/src/repl/eval.rs:482:13

# # # # # # # # #
#   Day04Part2  #
# # # # # # # # #

numsToStr : List (Num *) -> Str
numsToStr = \nums ->
    List.map nums Num.toStr |> Str.joinWith ","

printNums : List (Num *) -> Task {} *
printNums = \nums ->
    Stdout.line (numsToStr nums)

resultMapAndJoin : Result b a, (b -> a) -> a
resultMapAndJoin = \res, fn ->
    when res is
        Err val -> val
        Ok val -> fn val

FoundAndOthers a : { found: a, others: List a }

findAndPopLoop : List elem, List elem, (elem -> Bool) -> Result (FoundAndOthers elem) [ NotFound ]*
findAndPopLoop = \acc, list, pred ->
    { before, others: tl } = List.split list 1

    when List.first before is
        Err ListWasEmpty ->
            Err NotFound

        Ok found if pred found ->
            Ok { found, others: List.concat acc tl }

        Ok hd ->
            List.append acc hd |> findAndPopLoop tl pred


findAndPop : List elem, (elem -> Bool) -> Result (FoundAndOthers elem) [ NotFound ]*
findAndPop = \list, pred ->
    findAndPopLoop [] list pred
