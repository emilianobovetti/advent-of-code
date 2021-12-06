app "day02-part2"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.Stdout, base.File.{ Handle } ]
    provides [ main ] to base


Direction : [ Forward, Down, Up ]
Command : { direction: Direction, value: U64 }
AppError : [ EmptyCommand, UnknownCommand Str, InvalidNum Str ]
Coords : { position: I64, depth: I64, aim: I64 }


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


parseDirection : Result Str [ OutOfBounds ]* -> Result Direction AppError
parseDirection = \res ->
    when res is
        Ok key ->
            if key == "forward" then
                Ok Forward
            else if key == "down" then
                Ok Down
            else if key == "up" then
                Ok Up
            else
                Err (UnknownCommand key)

        Err OutOfBounds -> Err EmptyCommand


andThen : Result ok1 err, (ok1 -> Result ok2 err) -> Result ok2 err
andThen = \res, fn ->
    when res is
        Ok val -> fn val
        Err e -> Err e


parseCommand : Str -> Result Command AppError
parseCommand = \input ->
    cmdParts = Str.split input " "
    cmdKey = List.get cmdParts 0
    cmdValue = List.get cmdParts 1

    direction <- andThen (parseDirection cmdKey)
    value <- cmdValue |> Result.withDefault "" |> parseU64 |> Result.map

    { direction: direction, value: value}


updateCoords : Coords, Command -> Coords
updateCoords = \coords, cmd ->
    { position, depth, aim } = coords

    value : I64
    value = Num.intCast cmd.value

    when cmd.direction is
        Forward -> { coords & position: position + value, depth: depth + aim * value }
        Down -> { coords & aim: aim + value }
        Up -> { coords & aim: aim - value }


runCommands : Handle, Coords -> Task Coords AppError
runCommands = \handle, coords ->
    lineWithNL <- Task.await (File.line handle)
    line = Str.trim lineWithNL

    if line == "" then
        Task.succeed coords
    else when parseCommand line is
        Ok cmd -> runCommands handle (updateCoords coords cmd)
        Err e -> Task.fail e


main : Task {} *
main =
    handle <- File.withOpen "./data/day02-commands"
    result <- Task.attempt (runCommands handle { position: 0, depth: 0, aim: 0 })

    message =
        when result is
            Ok coords ->
                position = Num.toStr coords.position
                depth = Num.toStr coords.depth
                aim = Num.toStr coords.aim
                mult = Num.toStr (coords.position * coords.depth)
                "Result: position = \(position), depth = \(depth) aim = \(aim)\nposition * depth = \(mult)"

            Err EmptyCommand ->
                "Invalid empty command"

            Err (UnknownCommand str) ->
                "Unknown command: \(str)"

            Err (InvalidNum str) ->
                "Invalid number found: \(str)"


    {} <- Task.await (Stdout.line message)

    Task.succeed {}
