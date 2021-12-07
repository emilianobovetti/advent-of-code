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

numsToStr : List (Num *) -> Str
numsToStr = \list ->
    list |> List.map Num.toStr |> Str.joinWith ","

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
