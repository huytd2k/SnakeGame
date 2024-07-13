open System
open Snake

let renderBoard game =
    let { Snake = snake
          WindowSize = (height, width)
          Food = food } =
        game

    let mutable board = Array2D.create height width 0

    for (x, y) in snake do
        board.[x, y] <- 1

    let x, y = food
    board.[x, y] <- 2
    board

let printBoard board =
    let height, width = Array2D.length1 board, Array2D.length2 board

    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            printf "%d " board.[i, j]

        printfn ""

let tryGetInputEvent () =
    if Console.KeyAvailable then
        let key = Console.ReadKey(true)

        match key.Key with
        | ConsoleKey.UpArrow -> Some(Move Up)
        | ConsoleKey.DownArrow -> Some(Move Down)
        | ConsoleKey.LeftArrow -> Some(Move Left)
        | ConsoleKey.RightArrow -> Some(Move Right)
        | ConsoleKey.P -> Some Pause
        | _ -> None
    else
        None

let randomPosition (height, width) =
    let random = Random()
    (random.Next(height), random.Next(width))

let gameStep game =
    let inputEvent = tryGetInputEvent ()
    let updatedGame = game |> handleInputEvent inputEvent |> nextState (fun () -> randomPosition game.WindowSize)
    let board = renderBoard updatedGame
    (updatedGame, board)

let rec mainGameLoop game =
    Console.Clear()
    let updatedGame, board = gameStep game
    printBoard board
    printfn "Score: %d" updatedGame.Score
    System.Threading.Thread.Sleep(200)
    mainGameLoop updatedGame


mainGameLoop (initSnakeGameState ())
