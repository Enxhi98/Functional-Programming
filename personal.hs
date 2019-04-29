import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String
type Enemy = String

--Map
type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("gameStart", "n"), "greenRoom"),
    (("greenRoom", "e"), "redRoom"),
    (("redRoom", "w"), "greenRoom1"),
    (("redRoom", "s"), "blueRoom"),
    (("blueRoom", "s"), "blackRoom"),
    (("blackRoom", "w"), "whiteRoom"),
    (("whiteRoom", "c"), "whiteRoom1")
    ]

--Locations
type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("myself", "gameStart"),
    --("myself", "greenRoom"),
    --("myself", "whiteRoom"),
    ("diamond", "holding"),
    ("sword", "redRoom"),
    ("greenRoomAlive", "alive"),
    ("holding", "whiteRoom")

    ]
--Inventory of weapons
type Inventory = [Thing]
inventory :: Inventory
inventory = []

--Enemies
type EnemyMap =[(Enemy, Location)]
enemy :: EnemyMap
enemy = [
 ("giant", "redRoom"),
 ("zombie", "blueRoom"),
 ("spider", "blackRoom")
 ]


type World = (PathMap, LocationMap, Inventory, EnemyMap, Response)
world :: IO (PathMap, LocationMap, Inventory, EnemyMap, Response)
world = return (paths, locations, inventory, enemy, "")

main :: IO (String)
main = do
    putStrLn "\nWelcome to the room game!\n"
    putStrLn instructions
    play_game ( return (paths, locations, inventory,enemy, ""))
    return "Goodbye!"

--Game commands
instructions =
    "Enter commands using single letters or one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w         -- to go in that direction.\n" ++
    "take object        -- to pick up the named object.\n" ++
    "drop object        -- to put down the named object.\n" ++
    "use object         -- to use the named object.\n" ++
    "quit               -- to end the game and quit."

play_game :: IO (World) -> IO (World)
play_game world = do
    (paths, locations, inventory,enemy, response) <- world
    putStrLn response
    putStrLn ""
    do
      putStr "command> "
      command <- getLine
      if command == "quit"
         then return (paths, locations, inventory, enemy, "Quitting...")
         else play_game ( return (do_command command paths locations inventory enemy))

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> Inventory ->EnemyMap -> World
do_command "n" paths locations inventory enemy = go "n" paths locations inventory enemy
do_command "e" paths locations inventory enemy = go "e" paths locations inventory enemy
do_command "s" paths locations inventory enemy = go "s" paths locations inventory enemy
do_command "w" paths locations inventory enemy = go "w" paths locations inventory enemy
do_command "t" paths locations inventory enemy = takeItem paths locations inventory enemy
do_command "k" paths locations inventory enemy = killEnemy paths locations inventory enemy

killEnemy :: PathMap -> LocationMap -> Inventory -> EnemyMap -> World
killEnemy paths locations inventory enemy = do
  let my_location = get "myself" locations
  let enemy_location = get "giant" enemy
  if my_location == enemy_location then do
    (paths, locations, inventory, enemy, "You killed the giant, so you can go south")
  else (paths, locations, inventory, enemy, "No enemies to attack")


takeItem :: PathMap -> LocationMap -> Inventory -> EnemyMap ->World
takeItem paths locations inventory enemy = do
  let my_location = get "myself" locations
  let item_location = get "sword" locations
  let item_location_2 = get "diamond" locations
  if my_location == item_location then do
    let new_invetory = "sword" : inventory
    (paths, locations, new_invetory, enemy, "You take the Sword!")
  else if my_location == item_location_2 then do
    let new_invetory = "diamond" : inventory
    (paths, locations, new_invetory, enemy, "You take the Diamond and you Win!")
  else (paths, locations, inventory, enemy, "Nothing to take!")

go :: String -> PathMap -> LocationMap -> Inventory -> EnemyMap -> World
go direction paths locations inventory enemy = do
            let my_location = get "myself" locations
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, inventory, enemy, response)

{-west_from_blackRoom :: String -> PathMap -> LocationMap -> World
west_from_blackRoom direction paths locations =
    if get "myself" locations == "alive" &&
      "diamond" locations == "whiteRoom" &&
      "myself" locations == "whiteRoom" &&
      "myself" locations == "holding"
      then (paths, put "myself" "whiteRoom" locations, description "whiteRoom1")
      else go direction paths locations-}

get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "myself" locations
        greenRoom_status = get "greenRoomAlive" locations
        diamond_location = get "diamond" locations
    in describe_helper here greenRoom_status diamond_location  locations

describe_helper :: Location -> String -> String ->LocationMap -> String
describe_helper "greenRoom" "alive" _ locations = description "greenRoom"
describe_helper "whiteRoom" "alive" "holding" locations = description "whiteRoom"
describe_helper "redRoom" "alive" _ locations = description "redRoom"
describe_helper "blueRoom" "alive" _ locations = description "blueRoom"
describe_helper "blackRoom" "alive" _ locations = description "blackRoom"
describe_helper _ _ _ locations = description "null"

description :: Location -> String
description "greenRoom" =
    "You are in the green room. You need to reach the white room\n" ++
    "and collect the diamond to win; There is a red room\n" ++
    "to the east, but be careful as danger is near.\n"

description "redRoom" =
  "You are in the red room. Be quick to grab\n" ++
  "the sword nearby as a giant is approaching you. If u head back to the green room\n" ++
  "you will die. So hurry up and go south to the blue room."

description "greenRoom1" = "Oh no, you are dead. The game is over, you lost!"

description "blueRoom" =
    "You are in the blue room now. Although it appears as if there is \n" ++
    "no time to rest. Throw a fireball at it quickly and head south to\n" ++
    "the black room."

description "blackRoom" =
  "Ooh! This room is dark and smells odd.\n" ++
  "Ouch, was that a spider? Try to get out quickly by supporting yourself\n" ++
  "on the walls and head west to the white room."

description "whiteRoom" =
  "You are so close! You need to pick up the diamond.\n" ++
  "Head to the centre of the room."

description "whiteRoom1" = "Congratulations, you won the game!"

description _ = "you can't see anything."
