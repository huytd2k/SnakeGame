module Snake
// this module contains pure functions that are used to manipulate the game state

type Position = int * int
type Score = int
type Snake = Position list

type Direction =
    | Up
    | Down
    | Left
    | Right

type Height = int
type Width = int
type WindowSize = Height * Width

type SnakeGameState =
    { Snake: Snake
      Direction: Direction
      Food: Position
      Score: Score
      WindowSize: WindowSize }

type InputEvent =
    | Move of Direction
    | Pause

let reverseDirection direction =
    match direction with
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left


let handleInputEvent inputEvent game =
    let isValidDirection direction = 
        direction <> game.Direction && direction <> reverseDirection game.Direction

    match inputEvent with
    | Some(Move direction) when direction |> isValidDirection -> { game with Direction = direction }
    | Some Pause -> game // TODO: implement pause
    | _ -> game


let moveHead (direction: Direction) (windowsSize: WindowSize) : Position -> Position =
    let movePostition direction position =
        let x, y = position

        match direction with
        | Up -> (x - 1, y)
        | Down -> (x + 1, y)
        | Left -> (x, y - 1)
        | Right -> (x, y + 1)

    let normalizePos windowsSize pos =
        let x, y = pos
        let height, width = windowsSize
        let inline (%) x y = (x % y + y) % y // true modulo to modulo negative numbers

        (x % height, y % width)

    movePostition direction >> normalizePos windowsSize


let nextState nextFood game =
    let { Snake = snake
          Direction = direction
          Score = score
          WindowSize = windowsSize } =
        game

    let newHead = snake |> List.head |> moveHead direction windowsSize
    let foodEaten = (newHead = game.Food)
    let snakeWithoutTail = snake |> List.take (List.length snake - 1)

    let newSnake = newHead :: if foodEaten then snake else snakeWithoutTail

    { game with 
        Snake = newSnake ; 
        Food = if foodEaten then nextFood () else game.Food ;
        Score = if foodEaten then score + 1 else score
    }

let rec isSnakeCollidingWithItself snake =
    match snake with
        | [] -> false
        | head :: tail -> List.contains head tail 
    

let isGameOver game = 
    isSnakeCollidingWithItself game.Snake


let initSnakeGameState () =
    { Snake = [ (0, 0) ]
      Direction = Right
      Food = (1, 1)
      Score = 0
      WindowSize = (10, 10) }