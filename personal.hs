import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String

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

type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("myself", "gameStart")
    ("myself", "greenRoom"),
    ("myself", "whiteRoom")
    ("diamond", "whiteRoom"),
    ("sword", "redRoom"),
    ("fireball", "blueRoom"),
    ("hands", "blackRoom"),
    ("giant", "redRoom"),
    ("zombie", "blueRoom"),
    ("spider", "blackRoom"),
    ("dead", "greenRoom1"),
    ("holding", "whiteRoom"),
    ("alive", "whiteRoom")
    ]

type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")

main :: IO (String)
main = do
    putStrLn "\nWelcome to the room game!\n"
    putStrLn instructions
    play_game ( return (paths, locations, ""))
    return "Goodbye!"

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
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    do
      putStr "command> "
      command <- getLine
      if command == "quit"
         then return (paths, locations, "Quitting...")
         else play_game ( return (do_command command paths locations))

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "n" paths locations = go "n" paths locations
do_command "e" paths locations = go "e" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "w" paths locations = go "w" paths locations
--do_command "take object" paths locations =

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
            let my_location = get "myself" locations
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)

west_from_blackRoom :: String -> PathMap -> LocationMap -> World
west_from_blackRoom direction paths locations =
    if get "myself" locations == "alive" &&
      "diamond" locations == "whiteRoom" &&
      "myself" locations == "whiteRoom" &&
      "myself" locations == "holding"
      then (paths, put "myself" "whiteRoom" locations, description "whiteRoom1")
      else go direction paths locations

-- "get" finds the value of a key in a (key, value) list
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
        greenRoom_status = get "greenRoom" locations
        diamond_location = get "diamond" locations
    in describe_helper here greenRoom_status diamond_location  locations

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "greenRoom" "dead" "holding" locations = description "greenRoom1"
describe_helper "whiteRoom" "alive" "holding" locations = description "whiteRoom1"


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

description _ _ = "you can't see anything."
