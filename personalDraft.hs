import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String

type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("redRoom", "d"), "redRoom"),
    (("redRoom", "u"), "blueRoom"),
    (("redRoom", "w"), "redRoom entrance"),
    (("redRoom entrance", "e"), "redRoom"),
    (("redRoom entrance", "s"), "blueRoom"),
    (("blueRoom", "s"), "greenRoom"),
    (("blackRoom", "s"), "whiteRoom"),
    (("blueRoom", "n"), "redRoom entrance"),
    (("greenRoom", "n"), "blueRoom"),
    (("blueRoom", "e"), "blackRoom"),
    (("greenRoom", "w"), "blackRoom entrance"),
    (("blackRoom entrance", "e"), "greenRoom"),
    (("blackRoom", "u"), "redRoom"),
    (("blackRoom", "d"), "blueRoom"),
    (("whiteRoom", "e"), "greenRoom"),
    ]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("diamond", "whiteRoom"),
    ("key", "redRoom entrance"),
    ("myself", "blueRoom"),
    ("alive", "greenRoom"),
    ("torch", "blackRoom")
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
    "n  s  e  w  u  d   -- to go in that direction.\n" ++
    "take object        -- to pick up the named object.\n" ++
    "drop object        -- to put down the named object.\n" ++
    "use object        -- to use the named object.\n" ++
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
do_command "u" paths locations = go "u" paths locations
do_command "d" paths locations = down_from_greenRoom "d" paths locations
do_command "take object" paths locations = 

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
            let my_location = get "myself" locations
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)

down_from_room1 :: String -> PathMap -> LocationMap -> World
down_from_room1 direction paths locations =
    if get "myself" locations == "greenRoom" &&
       get "greenRoom" locations == "alive" &&
       get "diamond" locations == "holding"
           then (paths, put "myself" "dead" locations, description "room23")
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
describe_helper "blueRoom" "dead" "holding" locations = description "room23"
describe_helper "redRoom" "alive" "holding" locations = description "room23"
describe_helper "redRoom" "dead" _ locations = description "room22"
describe_helper "greenRoom" "dead" _ locations = description "room12"
describe_helper here "greenRoom" "alive" "holding" _ locations = description "room32"

description :: Location -> String
description "greenRoom" =
    "You are in a green room.  To the north is the blue room\n" ++
    "to the south is a white room.  Your\n" ++
    "assignment, should you decide to accept it, is to\n" ++
    "recover the world's most famous diamond and return it to\n" ++
    "this green room."

description "room32" = "Congratulations!!  You have recovered the diamond and won the game."

description "redRoom entrance" =
    "You are in the red room.  The exit is to\n" ++
    "the south towards the blue room; there is a dark spot to\n" ++
    "the east."

description "redRoom" =
    "There is a zombie here, ready to attack!\n" ++
    "I would advise you to leave promptly and quietly...."

description "room22" =
    "Yecch!  There is a big hairy spider here, coming towards you."

description "room23" =
     "The zombie sees you with the diamond and attacks!!!\n" ++
     "    ...it is over in seconds...."

description "greenRoom" =
    "You are underneath a leaking ceiling, standing on a \n" ++
    "slippery floor.  The smell is horrible."

description "room12" =
    "Oh another zombie's approaching you !"

description "blueRoom" =
    "This blue room is full of flowers and vibrant colours. \n" ++
    "Mmm, smells nice too. You need a challenge, go east \n" ++
    "towards the black room."

description "blackRoom" =
    "This room is so dark and there is a weird energy around it. \n" ++
    "You need some light, grab the torch quickly ! \n" ++
    "Head south, to go find some more light."

description "whiteRoom" =
    "Oh wow ! This looks like heaven. The music sounds so peaceful. \n" ++
    "Look ! There's the diamond. Pick it up and take it east to \n" ++
    "the white room to win the game."
