﻿module Oware

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
    | (false,true,false) -> "North won"
    | (false,false,true) -> "South won"
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
            match house_num<12 with
            | true -> 
                match (getSeeds house_num board)>0 with
                | true -> true
                | false -> check_houses (house_num+1)
            | false -> false 
        check_houses 7
    | 1 -> 
        let rec check_houses house_num =
            match house_num<6 with
            | true -> 
                match (getSeeds house_num board)>0 with
                | true -> true
                | false -> check_houses (house_num+1)
            | false -> false 
        check_houses 1
    | _ -> failwith "Invalid turn"
        

    
let harvest board num_seeds house_num = //harvest seeds
    let (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = board.houses
    let (South_seeds, North_seeds) = board.score 
    let n = 0
    match board.turn with
    | -1 -> 
        match house_num with
        | 7 -> {board with houses = (h1,h2,h3,h4,h5,h6,n,h8,h9,h10,h11,h12); score = (South_seeds+num_seeds, North_seeds)} 
        | 8 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,n,h9,h10,h11,h12); score = (South_seeds+num_seeds, North_seeds)} 
        | 9 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,n,h10,h11,h12); score = (South_seeds+num_seeds, North_seeds)} 
        |10 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,n,h11,h12); score = (South_seeds+num_seeds, North_seeds)} 
        |11 -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,n,h12); score = (South_seeds+num_seeds, North_seeds)} 
        |12 -> 
            match check_player_seeds board with
            |true -> {board with houses = (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,n); score = (South_seeds+num_seeds, North_seeds)} 
            |false -> board
        |_ -> board //can't capture seeds from own side 
    | 1 -> 
        match house_num with
        | 1 -> {board with houses = (n,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 2 -> {board with houses = (h1,n,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 3 -> {board with houses = (h1,h2,n,h4,h5,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 4 -> {board with houses = (h1,h2,h3,n,h5,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 5 -> {board with houses = (h1,h2,h3,h4,n,h6,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)} 
        | 6 ->
            match check_player_seeds board with
            | true -> {board with houses = (h1,h2,h3,h4,h5,n,h7,h8,h9,h10,h11,h12); score = (South_seeds, North_seeds+num_seeds)}
            |false -> board
        | _ -> board //can't capture seeds from own side
    | _ -> board

let harvestor board house_num = 
    let rec harvesting updateboard n =  
        match n with
        |0 -> 
            match getSeeds 12 updateboard with //does the last house we planted in contain two or three seeds?
            | 3 -> harvesting (harvest updateboard 3 12) 12 
            | 2 -> harvesting (harvest updateboard 2 12) 12      
            | _ -> {updateboard with turn = board.turn*(-1)}
        |_ -> 
            match getSeeds house_num updateboard with //does the last house we planted in contain two or three seeds?
            | 3 -> harvesting (harvest updateboard 3 n) (n-1) 
            | 2 -> harvesting (harvest updateboard 2 n) (n-1)  
            | _ -> {updateboard with turn = board.turn*(-1)}
    harvesting board house_num

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


let useHouse n board = 
    match valid_house_selected n board with
    | true -> 
        let numseeds = getSeeds n board
        let rec plantseeds seeds updateboard house_num =
            match seeds > 0 with //are there still seeds left to plant?
            | true -> //planting
                let numseeds = getSeeds house_num updateboard
                match house_num with //if house number  = 12 next house_num = 1
                | 12 -> plantseeds (seeds-1) (plant_or_harvest (numseeds+1) updateboard house_num) 1 
                | _ -> plantseeds (seeds-1) (plant_or_harvest (numseeds+1) updateboard house_num) (house_num+1) 
            | false -> harvestor updateboard (house_num-1)//work back through planted houses
        let board = plant_or_harvest 0 board n
        //let board = {board with turn = board.turn*(-1)}
        match (n+1)>12 with
        |false -> plantseeds numseeds board (n+1) //start recursive function
        |true -> plantseeds numseeds board (1) //start recursive function
    | false -> board
    

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

let score board = board.score

[<EntryPoint>]
let main _ = 
   
   let playGame numbers =
       let rec play xs game =
           match xs with
           | [] -> game
           | x::xs -> play xs (useHouse x game)
       play numbers (start South)
   //let game = playGame [2; 11; 3; 10; 4; 12; 1; 8; 6; 7; 5; 12; 2; 11; 1; 10]
   0 // return an integer exit code
