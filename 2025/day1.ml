let process_input input =
  let lines = String.split_on_char '\n' input in
  let parse_line line =
      let first_char = line.[0] in
      let rest = String.sub line 1 (String.length line - 1) in
      (first_char, int_of_string rest)
  in
  List.map parse_line lines

let rec solve pos acc = function
    | [] -> acc
    | ('L', length) :: t -> 
      let length = length mod 100 in
     let new_pos = 
      if length > pos then 
        100 - (length - pos)
      else pos - length
      in 
        let new_acc = 
          if new_pos == 0 then acc + 1 else acc
        in
        solve new_pos new_acc t 
    | ('R', length) :: t -> 
      let length = length mod 100 in
      let new_pos = 
        if length + pos < 100 then
          length + pos
        else length - (100 - pos)
      in 
        let new_acc = 
          if new_pos == 0 then acc + 1 else acc
        in solve new_pos new_acc t 
    | (dir, _) :: t -> failwith (Printf.sprintf "无效的方向: %c" dir)

let input = ""
let ans = input 
|> process_input 
|> solve 50 0
