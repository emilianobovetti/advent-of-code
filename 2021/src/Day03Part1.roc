app "day03-part1"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.Stdout, base.File.{ Handle } ]
    provides [ main ] to base


Bit : [ Zero, One ]

BitString : List Bit

## Key is an index, value counts
## how many "1" appear in that position
BitCount : Dict Nat Nat

AppError : [ InvalidBitString Str ]


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


updateBitCount : { index: Nat, bitCount: BitCount }, Bit -> { index: Nat, bitCount: BitCount }
updateBitCount = \{ index, bitCount }, bit ->
    onesCount : Nat
    onesCount =
        bitCount
        |> Dict.get index
        |> Result.withDefault 0

    updatedCount : Nat
    updatedCount =
        if bit == One then
            onesCount
        else
            onesCount + 1

    { index: index + 1, bitCount: Dict.insert bitCount index updatedCount }


countLineBits : BitCount, BitString -> BitCount
countLineBits = \bitCount, line ->
    res = List.walk line { index: 0, bitCount } updateBitCount

    res.bitCount


bitToU64 : Bit -> U64
bitToU64 = \bit ->
    when bit is
        Zero -> 0
        One -> 1


bitsToU64 : BitString -> U64
bitsToU64 = \bitString ->
    List.walk bitString 0 (\acc, bit -> acc * 2 + bitToU64 bit)


calcRate : List BitString, (Nat, Nat -> Bit) -> BitString
calcRate = \lines, calcBit ->
    linesCount : Nat
    linesCount = List.len lines

    onesCount : BitCount
    onesCount = List.walk lines Dict.empty countLineBits

    indexes : List Nat
    indexes =
        onesCount
        |> Dict.keys
        |> List.sortWith \x, y ->
            if x > y then GT
            else if x < y then LT
            else EQ

    zeroBits : BitString
    zeroBits =
        indexes
        |> List.len
        |> List.repeat Zero

    List.walk indexes zeroBits \acc, index ->
        ones = onesCount |> Dict.get index |> Result.withDefault 0
        zeros = linesCount - ones

        List.set acc index (calcBit zeros ones)


mainDiagnostic : Handle -> Task Str AppError
mainDiagnostic = \handle ->
    lines <- Task.await (readLines handle)

    gammaRate : BitString
    gammaRate = calcRate lines \zeros, ones ->
        if ones < zeros then
            One
        else
            Zero

    gammaRateDec : Str
    gammaRateDec = Num.toStr (bitsToU64 gammaRate)

    epsilonRate : BitString
    epsilonRate = calcRate lines \zeros, ones ->
        if ones > zeros then
            One
        else
            Zero

    epsilonRateDec : Str
    epsilonRateDec = Num.toStr (bitsToU64 epsilonRate)

    mult : Str
    mult =
        Num.toStr (bitsToU64 gammaRate * bitsToU64 epsilonRate)

    messages : List Str
    messages = [
            "gamma rate = \(gammaRateDec)",
            "epsilon rate = \(epsilonRateDec)",
            "gamma rate * epsilon rate = \(mult)"
        ]

    Task.succeed (Str.joinWith messages "\n")


errorToString : AppError -> Str
errorToString = \error ->
    when error is
        InvalidBitString str -> "Invalid bitstring found: \(str)"


joinResult : Result a a -> a
joinResult = \res ->
    when res is
        Ok v -> v
        Err e -> e


main : Task {} *
main =
    handle <- File.withOpen "./data/day03-diag"
    result <- Task.attempt (mainDiagnostic handle)

    {} <-
        result
        |> Result.mapErr errorToString
        |> joinResult
        |> Stdout.line
        |> Task.await

    Task.succeed {}
