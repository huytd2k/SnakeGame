open System
// For more information see https://aka.ms/fsharp-console-apps

type Position = int * int
type Score = int
type Snake = Position list

type Direction = Up | Down | Left | Right

type SnakeGameState = {
    snake: Snake
    direction: Direction
    food: Position
    score: Score
}

let initSnakeGameState () = {
    snake = [(0, 0)]
    direction = Right
    food = (1, 1)
    score = 0
}

type Height = int
type Width = int
type GameWindowSize = Height * Width

let initGameWindowSize () = (10, 10)

type Game = {
    state: SnakeGameState
    gameWindowSize: GameWindowSize
}

let initGame () = {
    state = initSnakeGameState ()
    gameWindowSize = initGameWindowSize ()
}   


let renderBoard game = 
    let snake = game.state.snake
    let food = game.state.food
    let height, width = game.gameWindowSize
    let mutable board = Array2D.create height width 0
    for (x, y) in snake do
        board.[x, y] <- 1
    let x, y = food
    board.[x, y] <- 2
    board

let printBoard board = 
    let height, width = Array2D.length1 board, Array2D.length2 board
    for i in 0..height - 1 do
        for j in 0..width - 1 do
            printf "%d " board.[i, j]
        printfn ""

let movehead direction head = 
    let x, y = head

    match direction with
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)
    | Left -> (x, y - 1)
    | Right -> (x, y + 1)

let inline (%) x y = (x % y + y) % y

let normalizePos (height, width) pos = 
    let x, y = pos
    (x % height, y % width)

let rec moveSnake game = 
    let snake = game.state.snake
    let head = List.head snake
    let newHead = head |> movehead game.state.direction |> normalizePos game.gameWindowSize

    { game with state = { game.state with snake = newHead :: snake |> List.rev |> List.tail } }


type InputEvent = 
    | Move of Direction
    | Pause

let tryGetInputEvent () = 
    if Console.KeyAvailable then
        let key = Console.ReadKey (true)
        match key.Key with
        | ConsoleKey.UpArrow -> Some (Move Up)
        | ConsoleKey.DownArrow -> Some (Move Down)
        | ConsoleKey.LeftArrow -> Some (Move Left)
        | ConsoleKey.RightArrow -> Some (Move Right)
        | ConsoleKey.P -> Some Pause
        | _ -> None
    else
        None

let handleInputEvent inputEvent game = 
    match inputEvent with
    | Some (Move direction) -> { game with state = { game.state with direction = direction } }
    | Some Pause -> game
    | None -> game

let gameStep game = 
    let inputEvent = tryGetInputEvent ()
    let updatedGame = game |> handleInputEvent inputEvent |> moveSnake
    let board = renderBoard updatedGame
    (updatedGame, board)

let rec mainGameLoop game = 
    let updatedGame, board = gameStep game
    Console.Clear()
    printBoard board
    System.Threading.Thread.Sleep(200)
    mainGameLoop updatedGame


mainGameLoop (initGame ())