module Oware

type StartingPosition =
    | South
    | North

type Board = {
    houses: int*int*int*int*int*int*int*int*int*int*int*int  //Houses 1-12  Houses 1-6 belong to South player and Houses 7-12 belong to North player
    score : int*int // (south, north)
    turn : int //checks whos turn it is -1 = South's turn whereas 1 = North's turn
    }

let getSeeds n board = 
    let (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = board.houses
    match n with
    | 1 -> h1| 2 -> h2 | 3 -> h3 | 4 -> h4 | 5 -> h5 | 6 -> h6 
    | 7 -> h7 | 8 -> h8 | 9 -> h9 | 10 -> h10 | 11 -> h11 | 12 -> h12 | _ -> failwith "Invalid number on board"

let gameState board =  //Checks the state of the game and returns the state at which the game is in
    let (northplayer,southplayer) = board.score 
    match (northplayer=24 && southplayer=24),(northplayer>24 && southplayer<24),(northplayer<24 && southplayer>24) with
    | (true, false,false) -> "Game ended in a draw"
    | (false,true,false) -> "South won" //Inverted 
    | (false,false,true) -> "North won" //Inverted
    | _ ->
        match board.turn with
        | 1 -> "North's turn"
        | -1 -> "South's turn"
        | _ -> failwith "invalid"

//used with useHouse
let plant_or_harvest n board house_num = //n= number of seeds meant to be in the specified house number
    let (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = board.houses
    match house_num with
    | 1 -> {board with houses = (n,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} 
    | 2 -> {board with houses = (h1,n,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} 
    | 3 -> {board with houses = (h1,h2,n,h4,h5,h6,h7,h8,h9,h10,h11,h12)} 
    | 4 -> {board with houses = (h1,h2,h3,n,h5,h6,h7,h8,h9,h10,h11,h12)} 
    | 5 -> {board with houses = (h1,h2,h3,h4,n,h6,h7,h8,h9,h10,h11,h12)} 
    | 6 -> {board with houses = (h1,h2,h3,h4,h5,n,h7,h8,h9,h10,h11,h12)} 
    | 7 -> {board with houses = (h1,h2,h3,h4,h5,h6,n,h8,h9,h10,h11,h12)} 
    | 8 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,n,h9,h10,h11,h12)} 
    | 9 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,n,h10,h11,h12)} 
    |10 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,n,h11,h12)} 
    |11 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,n,h12)} 
    |12 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,n)} 
    |_ -> failwith "Invalid house number"

let check_player_seeds board =  //checks that a move of seeds in final house doesn't result in no seeds left on that players side
    match board.turn with
    | -1 -> 
        let rec check_houses house_num =
            match house_num<13 with
            | true -> 
                match (getSeeds house_num board)>0 with
                | true -> true
                | false -> check_houses (house_num+1)
            | false -> false 
        check_houses 7
    | 1 -> 
        let rec check_houses house_num =
            match house_num<7 with
            | true -> 
                match (getSeeds house_num board)>0 with
                | true -> true
                | false -> check_houses (house_num+1)
            | false -> false 
        check_houses 1
    | _ -> failwith "Invalid turn"
        
let valid_house_selected house_num board = //checkes if the player selected a valid house belonging to them and the house has seeds in it
    match board.turn with
    | -1 -> 
        match house_num>=1 && house_num <=6 && getSeeds house_num board > 0  with
        | true -> true
        | false -> false
    | 1 -> 
        match house_num>=7 && house_num <=12 && getSeeds house_num board > 0 with
            | true -> true
            | false -> false
    | _ -> false
    
let harvest board num_seeds house_num = //harvest seeds
    let (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = board.houses
    let (South_seeds, North_seeds) = board.score 
    let n = 0
   // printfn ("GHere: %d") num_seeds
    match board.turn with
    | -1 -> 
        match house_num with
        | 7 -> {board with houses = (h1,h2,h3,h4,h5,h6,0,h8,h9,h10,h11,h12); score = (South_seeds+num_seeds, North_seeds)} 
        | 8 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,0,h9,h10,h11,h12); score = (South_seeds+num_seeds, North_seeds)} 
        | 9 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,0,h10,h11,h12); score = (South_seeds+num_seeds, North_seeds)} 
        |10 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,0,h11,h12); score = (South_seeds+num_seeds, North_seeds)} 
        |11 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,0,h12); score = (South_seeds+num_seeds, North_seeds)} 
        |12 -> 
            match check_player_seeds board with
            |true -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,0); score = (South_seeds+num_seeds, North_seeds)} 
            |false -> board
        |_ -> board //can't capture seeds from own side 
    | 1 -> 
        match house_num with
        | 1 -> {board with houses = (0,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 2 -> {board with houses = (h1,0,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 3 -> {board with houses = (h1,h2,0,h4,h5,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 4 -> {board with houses = (h1,h2,h3,0,h5,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 5 -> {board with houses = (h1,h2,h3,h4,0,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 6 ->
            match check_player_seeds board with
            | true -> {board with houses = (h1,h2,h3,h4,h5,0,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)}
            |false -> board
        | _ -> board //can't capture seeds from own side
    | _ -> board

let harvestor board house_num = 
    let rec harvesting updateboard n =  
        match n with
        |0 -> 
            match getSeeds 12 updateboard with //does the last house we planted in contain two or three seeds?
            | 3 ->
                printfn("\nHere3     %d\n ") n
                harvesting (harvest updateboard 3 12) (11) 
            | 2 ->
                printfn("\nHere2     %d\n") n
                harvesting (harvest updateboard 2 12) (11)      
            | _ -> {updateboard with turn = board.turn*(-1)}
        |_ -> 
            match getSeeds n updateboard with //does the last house we planted in contain two or three seeds?
            | 3 ->
                printfn("\nHere3     %d\n ") n
                harvesting (harvest updateboard 3 n) (n-1) 
            | 2 ->
                printfn("\nHere2     %d\n") n
                harvesting (harvest updateboard 2 n) (n-1)
                
            | _ -> {updateboard with turn = board.turn*(-1)}
    harvesting board house_num

let give_opponent_pieces board = 
    match board.turn with
    | -1 -> 
        let rec check_houses house_num k =
            match house_num>=1 with
            | true -> 
                match (getSeeds house_num board)>=k with
                | true -> house_num
                | false -> check_houses (house_num-1) (k+1)
            | false -> failwith "Cannot play to give opponent pieces" 
        check_houses 6 1
    | 1 -> 
        let rec check_houses house_num k =
            match house_num>=7 with
            | true -> 
                match (getSeeds house_num board)>=k with
                | true -> house_num
                | false -> check_houses (house_num-1) (k+1)
            | false -> failwith "Cannot play to give opponent pieces"
        check_houses 12 1
    | _ -> failwith "Invalid turn"    


let useHouse n board = 
    match check_player_seeds board with 
    | true ->
        match valid_house_selected n board with
        | true -> 
            let numseeds = getSeeds n board
            let rec plantseeds seeds updateboard house_num origin=
                match seeds > 0 with //are there still seeds left to plant?
                | true -> //planting
                    let numseeds = getSeeds house_num updateboard
                    match house_num with //if house number  = 12 next house_num = 1
                    | 12 -> 
                        printfn ("Here.....: %d: origin: %d") house_num origin
                        match origin = house_num with
                        |true -> 
                            printfn ("True test\n\n\n\n\n\n\nt")
                            plantseeds (seeds-1) (plant_or_harvest ((getSeeds (house_num+1) updateboard)+1) updateboard (house_num+1)) 1 origin
                        |false -> plantseeds (seeds-1) (plant_or_harvest (numseeds+1) updateboard house_num) 1 origin
                    | _ ->
                        match origin = house_num with
                        | true -> 
                            printfn ("True test\n\n\n\n\n\n\nt")
                            plantseeds (seeds-1) (plant_or_harvest ((getSeeds (house_num+1) updateboard)+1) updateboard (house_num+1)) (house_num+1) origin
                        | false -> plantseeds (seeds-1) (plant_or_harvest (numseeds+1) updateboard house_num) (house_num+1) origin
                | false ->
                    printfn ("Here Seed at %d") house_num
                    harvestor updateboard (house_num-1)//work back through planted houses
            let board = plant_or_harvest 0 board n
            //let board = {board with turn = board.turn*(-1)}
            match (n+1)>12 with
            |false -> plantseeds numseeds board (n+1) n//start recursive function
            |true -> plantseeds numseeds board (1) n//start recursive function
        | false -> board
    | false -> 
        let rec plantseeds seeds updateboard house_num origin=
            match seeds > 0 with //are there still seeds left to plant?
            | true -> //planting
                let numseeds = getSeeds house_num updateboard
                match house_num with //if house number  = 12 next house_num = 1
                | 12 -> 
                    printfn ("Here.....: %d: origin: %d") house_num origin
                    match origin = house_num with
                    |true -> 
                        printfn ("True test\n\n\n\n\n\n\nt")
                        plantseeds (seeds-1) (plant_or_harvest ((getSeeds (house_num+1) updateboard)+1) updateboard (house_num+1)) 1 origin
                    |false -> plantseeds (seeds-1) (plant_or_harvest (numseeds+1) updateboard house_num) 1 origin
                | _ ->
                    match origin = house_num with
                    | true -> 
                        printfn ("True test\n\n\n\n\n\n\nt")
                        plantseeds (seeds-1) (plant_or_harvest ((getSeeds (house_num+1) updateboard)+1) updateboard (house_num+1)) (house_num+1) origin
                    | false -> plantseeds (seeds-1) (plant_or_harvest (numseeds+1) updateboard house_num) (house_num+1) origin
            | false ->
                printfn ("Here Seed at %d") house_num
                harvestor updateboard (house_num-1)//work back through planted houses\
        //let n = give_opponent_pieces board
        let newhouse = give_opponent_pieces board
        let numseeds = getSeeds newhouse board
        let board = plant_or_harvest 0 board (give_opponent_pieces board)
        plantseeds numseeds board (newhouse+1) newhouse
        //match (newhouse+1)>12 with
        //|false -> plantseeds numseeds board (newhouse+1) newhouse //start recursive function
        //|true -> plantseeds numseeds board (1) newhouse//start recursive function
        //plantseeds numseeds board newhouse newhouse
       
    

let printBoard board= //mini print function for testing
    let i = 1
    let rec printer i board=
        match i < 13 with
        | true -> 
            let x = getSeeds i board
            printer (i+1) board
            printfn "House %d: %d" i x
        | false -> ()
    printer i board

let getScore n board =
    let (x,y) = board.score
    match n with
    | 1 -> x
    | 2 -> y
    |_ -> failwith "Incorrect player"
  
let start position = 
    let initial = {houses=(4,4,4,4,4,4,4,4,4,4,4,4);score=(0,0); turn= -1}
    match position with
    | North -> {initial with turn=1}
    | South -> {initial with turn=(-1)}

let printScore board =
    let i = 1
    let rec printer i board =
        match i<3 with
        |true ->
            let x = getScore i board
            printer (i+1) board
            printfn "Player %d: %d" i x
        |false -> ()
    printer i board

let score board = board.score

[<EntryPoint>]
let main _ = 
   let playGame numbers =
       let rec play xs game =
           printBoard game
           printfn ("\n")
           printScore game
           printfn("\n")
           match xs with
           | [] -> game
           | x::xs -> play xs (useHouse x game)
       play numbers (start South)
   let game = playGame [6; 8; 5; 9; 4; 12; 3; 10; 1; 11; 2; 12; 5; 7; 5; 11; 6; 8; 1; 12; 4; 10; 5; 9;
   2; 11; 3; 12; 6; 9; 5; 10; 2; 11; 1; 12; 4; 7; 6; 7; 3; 8; 5; 9; 6; 10; 1; 11;
   2; 12; 3; 8; 4; 9; 1; 10; 2; 11; 3; 12; 1]
   printBoard game
   let x = gameState game
   printScore game
   printfn("%s") x
   0 // return an integer exit code
