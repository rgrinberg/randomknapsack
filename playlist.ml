module type Songlist = sig
  type song
  val pool : song array
  val fitness : int BatSet.t -> int
  val pool_size : int
  val step_selector : generation:int -> playlist:(int BatSet.t) -> int
end

module PlaylistMaker (S : Songlist) = struct
  open Batteries

  let empty_playlist = BatSet.empty

  let dist p1 p2 =  abs (p1 - p2)

  let random_song ~playlist = 
    (*keep trying to draw element until we succeed*)
    let rec loop () = 
      let rnd = (Random.int S.pool_size) in
      if (playlist |> BatSet.mem rnd) 
      then loop ()
      else rnd
    in loop ()

  let remove_random ~playlist =
    try BatSet.remove (BatSet.enum playlist |> Random.choice) playlist
    (*
     *sometimes we got some exception here probably because we are trying to
     *remove an element from an empty playlist
     *)
    with _ -> playlist

  let add_random ~playlist = playlist |> BatSet.add (random_song ~playlist)

  let change_playlist ~playlist ~steps ~f = 
    let rec loop step acc = 
      if step = 0 then acc
      else loop (pred step) (f ~playlist:acc)
    in loop steps playlist

  let random_playlist ~size = 
    let rec loop s pl =
      if s = 0 then pl
      else loop (pred s) (add_random pl)
    in loop size empty_playlist

  let mutate_playlist ~playlist ~goal ~steps = 
    let fit = S.fitness playlist in
    let new_pl = 
      if fit > goal then change_playlist ~playlist ~steps ~f:remove_random
      else change_playlist ~playlist ~steps ~f:add_random
    in
    let new_fit = S.fitness new_pl in
    if (dist new_fit goal) < (dist fit goal) then new_pl
    else playlist

  let breed ~playlists ~goal ~generations = 
    let rec loop generation pls = 
      if generation = 0 then pls
      else loop (pred generation)
      (pls |> List.map (fun playlist -> mutate_playlist ~playlist ~goal
      ~steps:(S.step_selector ~playlist ~generation)))
    in loop generations playlists

  let convert_playlist ~playlist = playlist |> BatSet.enum |> Enum.map (fun x ->
    S.pool.(x))
end

module SimpleSongList = struct
  type song = {
    name : string;
    length : int; }
  let random_songname () = 
    Array.init 8 (fun _ ->
      (97 + (Random.int 25)) |> char_of_int )
      |> Array.map String.of_char |> Array.reduce (^)
  let pool = (1--1000) |> Enum.map ( fun _ -> 
    { length=(Random.int (60 * 8)) + (60 * 2) ; name=random_songname() }) |> Array.of_enum
  let fitness pl = BatSet.fold (fun e a -> 
    match pool.(e) with
    | { name; length } -> length + a ) pl 0
  let pool_size = pool |> Array.length
  let step_selector ~generation ~playlist =
    match generation with
    | _ when generation < 500 -> 1
    | _ when generation < 900 -> 2
    | _ when generation < 1000 -> 3
    | _ -> 4

  let string_of_song = function
    | { name ; length } -> name ^ "//" ^ (string_of_int length)
end

module SSL = (SimpleSongList:Songlist with type song = SimpleSongList.song)

module PL = PlaylistMaker(SSL) 
let _ = 
  let playlists =
    (1--50) |> Enum.map (fun size -> PL.random_playlist ~size) |> List.of_enum
  in 
  let goal = 60 * 45 in
  let df x = abs (goal - x) in
  (PL.breed ~playlists ~goal ~generations:50)
    |> List.map (fun pl -> (SimpleSongList.fitness pl, pl))
    |> List.sort (fun (a,_) (b,_) -> compare (df a) (df b))
    |> List.take 5
    |> List.map (fun (a,x) -> 
        (df a, (PL.convert_playlist ~playlist:x)
                |> Enum.map SimpleSongList.string_of_song 
                |> List.of_enum) )
