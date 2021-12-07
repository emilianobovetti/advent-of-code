app "day03-part2"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.Stdout, base.File.{ Handle } ]
    provides [ main ] to base


Bit : [ Zero, One ]

BitString : List Bit

BitPartition : { withZero: List BitString, withOne: List BitString }

AppError : [ InvalidBitString Str, NotEnoughData ]


parseBits : List U8, BitString -> Result BitString {}
parseBits = \bytes, line ->
    { before, others: tl } = List.split bytes 1

    when List.first before is
        Err ListWasEmpty ->
            Ok line

        Ok char if char == 48 ->
            parseBits tl (List.append line Zero)

        Ok char if char == 49 ->
            parseBits tl (List.append line One)

        Ok _ ->
            Err {}


parseLine : Str -> Result BitString AppError
parseLine = \str ->
    str
    |> Str.toUtf8
    |> parseBits []
    |> Result.mapErr (\{} -> InvalidBitString str)


readLinesLoop : Handle, List BitString -> Task (List BitString) AppError
readLinesLoop = \handle, lines ->
    lineWithNL <- Task.await (File.line handle)
    line = Str.trim lineWithNL

    if line == "" then
        Task.succeed lines
    else when parseLine line is
        Ok bits -> readLinesLoop handle (List.append lines bits)
        Err e -> Task.fail e


readLines : Handle -> Task (List BitString) AppError
readLines = \handle ->
    readLinesLoop handle []


bitToU64 : Bit -> U64
bitToU64 = \bit ->
    when bit is
        Zero -> 0
        One -> 1


bitsToU64 : BitString -> U64
bitsToU64 = \bitString ->
    List.walk bitString 0 (\acc, bit -> acc * 2 + bitToU64 bit)


partitionByBit : List BitString, Nat -> BitPartition
partitionByBit = \lines, position ->
    List.walk lines { withZero: [], withOne: [] } \acc, bitString ->
        when List.get bitString position is
            Ok Zero -> { acc & withZero: List.append acc.withZero bitString }
            Ok One -> { acc & withOne: List.append acc.withOne bitString }
            Err OutOfBounds -> acc


errorToString : AppError -> Str
errorToString = \error ->
    when error is
        InvalidBitString str -> "Invalid bitstring found: \(str)"
        NotEnoughData -> "There is not enough data to find ratings"


joinResult : Result a a -> a
joinResult = \res ->
    when res is
        Ok v -> v
        Err e -> e


lifeSupportRating : List BitString, Nat, (BitPartition -> List BitString) -> Result BitString AppError
lifeSupportRating = \lines, position, bitCriteria ->
    when List.first lines is
        Err ListWasEmpty ->
            Err NotEnoughData

        Ok line if List.len lines == 1 ->
            Ok line

        Ok _ ->
            lines
            |> partitionByBit position
            |> bitCriteria
            |> lifeSupportRating (position + 1) bitCriteria


oxygenGeneratorRating : List BitString -> Result BitString AppError
oxygenGeneratorRating = \lines ->
    { withZero, withOne } <- lifeSupportRating lines 0

    if List.len withOne >= List.len withZero then
        withOne
    else
        withZero


co2ScrubberRating : List BitString -> Result BitString AppError
co2ScrubberRating = \lines ->
    { withZero, withOne } <- lifeSupportRating lines 0

    if List.len withZero <= List.len withOne then
        withZero
    else
        withOne


findRatings : Handle -> Task Str AppError
findRatings = \handle ->
    lines <- Task.await (readLines handle)

    oxygrBits <-
        lines
        |> oxygenGeneratorRating
        |> Task.fromResult
        |> Task.await

    oxygrDec = bitsToU64 oxygrBits
    oxygrStr = Num.toStr oxygrDec

    co2srBits <-
        lines
        |> co2ScrubberRating
        |> Task.fromResult
        |> Task.await

    co2srDec = bitsToU64 co2srBits
    co2srStr = Num.toStr co2srDec

    lsr = Num.toStr (oxygrDec * co2srDec)

    [
        "oxygen generator rating = \(oxygrStr)",
        "CO2 scrubber rating = \(co2srStr)",
        "life support rating = \(lsr)",
    ]
    |> Str.joinWith "\n"
    |> Task.succeed


main : Task {} *
main =
    handle <- File.withOpen "./data/day03-diag"
    result <- Task.attempt (findRatings handle)

    {} <-
        result
        |> Result.mapErr errorToString
        |> joinResult
        |> Stdout.line
        |> Task.await

    Task.succeed {}
