app "day01-part2"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.Stdout, base.File.{ Handle } ]
    provides [ main ] to base


ParseResult : [ Parsed U64, StrWasEmpty, NonDigitFound U8 ]


parseU64Loop : List U8, U64 -> ParseResult
parseU64Loop = \bytes, acc ->
    { before, others: tl } = List.split bytes 1

    when List.first before is
        Err ListWasEmpty ->
            Parsed acc

        Ok char if char >= 48 && char <= 57 ->
            parseU64Loop tl (acc * 10 + Num.intCast char - 48)

        Ok char ->
            NonDigitFound char


parseU64 : Str -> ParseResult
parseU64 = \str ->
    if str == "" then
        StrWasEmpty
    else
        parseU64Loop (Str.toUtf8 str) 0


AppError : [ NotEnoughMeasurements, NonNumericLine Str ]


countIncreasesLoop : Handle, U64, U64, U64, U64 -> Task U64 AppError
countIncreasesLoop = \handle, first, second, third, count ->
    lineWithNL <- Task.await (File.line handle)

    line = Str.trim lineWithNL

    when parseU64 line is
        Parsed num ->
            if second + third + num > first + second + third then
                countIncreasesLoop handle second third num (count + 1)
            else
                countIncreasesLoop handle second third num count

        StrWasEmpty ->
            Task.succeed count

        NonDigitFound _ ->
            Task.fail (NonNumericLine line)


getNum : List U64, Nat -> U64
getNum = \list, index ->
    list
    |> List.get index
    |> Result.withDefault 0


initCountIncreases : Handle, List U64 -> Task U64 AppError
initCountIncreases = \handle, measurements ->
    if List.len measurements < 3 then
        lineWithNL <- Task.await (File.line handle)

        line = Str.trim lineWithNL

        when parseU64 line is
            Parsed num ->
                initCountIncreases handle (List.prepend measurements num)

            StrWasEmpty ->
                Task.fail NotEnoughMeasurements

            NonDigitFound _ ->
                Task.fail (NonNumericLine line)
    else
        first = getNum measurements 0
        second = getNum measurements 1
        third = getNum measurements 2

        countIncreasesLoop handle first second third 0


countSlidingWindowIncreases : Handle -> Task U64 AppError
countSlidingWindowIncreases = \handle ->
    initCountIncreases handle []


main : Task {} *
main =
    handle <- File.withOpen "./data/day01-depths"
    result <- Task.attempt (countSlidingWindowIncreases handle)

    printRes =
        when result is
            Ok count ->
                str = Num.toStr count
                Stdout.line "Depth increases count: \(str)"

            Err NotEnoughMeasurements ->
                Stdout.line "There aren't enough measurements"

            Err (NonNumericLine str) ->
                Stdout.line "Line with non-digit char found: \(str)"

    {} <- Task.await printRes

    Task.succeed {}
