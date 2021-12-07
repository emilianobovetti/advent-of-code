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
    List.walk bits "" \acc, bit ->
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
