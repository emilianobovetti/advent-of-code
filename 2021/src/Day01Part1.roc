app "day01-part1"
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


AppError : [ EmptyFile, NonNumericLine Str ]


countIncreasesLoop : Handle, U64, U64 -> Task U64 AppError
countIncreasesLoop = \handle, lastNum, count ->
    lineWithNL <- Task.await (File.line handle)

    line = Str.trim lineWithNL

    when parseU64 line is
        Parsed currentNum ->
            countIncreasesLoop handle currentNum
                (if currentNum > lastNum then count + 1 else count)

        StrWasEmpty ->
            Task.succeed count

        NonDigitFound _ ->
            Task.fail (NonNumericLine line)


countIncreases : Handle -> Task U64 AppError
countIncreases = \handle ->
    firstLineWithNL <- Task.await (File.line handle)

    firstLine = Str.trim firstLineWithNL

    when parseU64 firstLine is
        Parsed firstNum ->
            countIncreasesLoop handle firstNum 0

        StrWasEmpty ->
            Task.fail EmptyFile

        NonDigitFound _ ->
            Task.fail (NonNumericLine firstLine)


main : Task {} *
main =
    handle <- File.withOpen "./data/day01-depths"
    result <- Task.attempt (countIncreases handle)

    printRes =
        when result is
            Ok count ->
                str = Num.toStr count
                Stdout.line "Depth increases count: \(str)"

            Err EmptyFile ->
                Stdout.line "Input file was empty"

            Err (NonNumericLine str) ->
                Stdout.line "Line with non-digit char found: \(str)"

    {} <- Task.await printRes

    Task.succeed {}
