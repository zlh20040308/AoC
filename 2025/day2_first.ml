let process_input input =
  let ranges = String.split_on_char ',' input in
  let parse_range range =
      match String.split_on_char '-' range with 
      | [a; b] -> (int_of_string a, int_of_string b)
      | _ -> failwith "Invalid range format"
  in
  List.map parse_range ranges

let range a b =
  let rec aux a b =
    if a > b then [] else a :: aux (a + 1) b
  in
    if a > b then List.rev (aux b a) else aux a b

let is_invalid_id id = 
  let len = String.length id in
  if len mod 2 == 1 then
    false
  else 
    let half = len / 2 in
    let left = String.sub id 0 half in
    let right = String.sub id half half in
    if left = right then true else false

let acc_invalid_id lst = 
  List.fold_left (fun acc int_num -> 
    let str_num = string_of_int int_num in 
    if is_invalid_id str_num then
      int_num + acc 
    else
      acc
  ) 0 lst
let input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

let rec solve input = 
  List.fold_left (fun acc (a, b) -> 
    ((range a b) |> acc_invalid_id) + acc
  ) 0 input 

let ans = input |> process_input |> solve