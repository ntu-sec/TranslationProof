let rec pop_n (l : 'a list) (n : int) =
  match n > 0 with
  | true -> (match l with
             | h :: l' -> let (values, result) = pop_n (l') (n-1) in
                          (h::values, result)
             | [] -> raise (Invalid_argument "Attempting to pop an empty stack"))
  | false -> ([], l);;

let rec list_to_StringMap (l : (string * 'a) list) =
  match l with
  | [] -> Map_type.StringMap.empty
  | (str, attr) :: t -> Map_type.StringMap.add (str) (attr) (list_to_StringMap (t));;

let rec list_to_intMap (l : (int * 'a) list) =
  match l with
  | [] -> Map_type.Int32Map.empty
  | (key, attr) :: t -> Map_type.Int32Map.add (Int32.of_int key) (attr) (list_to_intMap (t));;

let readChar (ic:in_channel) = Scanf.fscanf ic "%c" (fun len -> len);;
let readByte (ic: in_channel) = Scanf.fscanf ic "%c" (fun len -> Char.code len);;

let rec read_n (fn:in_channel -> 'a) (ic:in_channel) (n:int) =
  match n <= 0 with
  | true -> []
  | false -> let current_value = fn (ic) in
             current_value :: read_n (fn) (ic) (n - 1)
;;

let rec int_iterator (n : int) : int list =
  match n >= 0 with
  | false -> []
  | true -> n :: int_iterator (n - 1);;

let readShort (ic: in_channel) =
  let n1 = readByte(ic) in
  let n2 = readByte(ic) in
  (n1 * 256) + n2;;

let readInt (ic: in_channel) =
  let n1 = readShort (ic) in
  let n2 = readShort (ic) in
  (n1 * 256 * 256) + n2;;

let signed_byte (value:int) =
  if (value > 127) then value - 256
  else value;;

let signed_int (value:int) =
  if (value > 32767) then value - 65536
  else value;;

let string_of_byte_value (i:int) = String.make 1 (Char.chr i);;

let string_of_short_value (i:int) =
  (String.make 1 (Char.chr (i lsr 8))) ^ (String.make 1 (Char.chr (i land 255)));;

let rec string_of_byte_list_rec (inp : int list) =
  match inp with
  | [] -> ""
  | h :: t -> string_of_byte_value (h) ^ string_of_byte_list_rec (t);;
let string_of_byte_list (inp : int list) =
  string_of_byte_value (List.length inp) ^ string_of_byte_list_rec (inp);;

let rec print_list (lst) (fn) =
  match lst with
    [] -> ()
  | h :: t -> (fn h); print_list (t) (fn);;

let rec string_of_int_list (lst) (fn) =
  match lst with
  | [] -> ""
  | h :: t -> (fn h) ^ string_of_int_list (t) (fn);;

(* parsing method's prototype to get how many arguments there are, based   *)
(* on signature detailed in oracle's documentation on Java                 *)
let rec countArgs (str: string) (len: int) (count: int) =
  match str.[0] with
    'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' ->
                                               (match (len - 1) <= 0 with
                                                  true -> (count + 1)
                                                | false -> countArgs (String.sub (str) (1) (len - 1)) (len - 1) (count + 1)
                                               )
    | '[' -> countArgs (String.sub (str) (1) (len - 1)) (len - 1) (count)
    | 'L' -> (let nextIndex = (String.index (str) (';')) + 1 in
              match nextIndex >= len with
                true -> (count + 1)
              | false -> countArgs (String.sub (str) (nextIndex) (len - nextIndex)) (len - nextIndex) (count + 1)
             )
    | _ as chr -> raise (Invalid_argument ("Invalid character for a method descriptor : " ^ Char.escaped (chr)))
;;

let countRet_from_desc (desc:string) =
  let close = String.index (desc) (')') in
  let length = String.length (desc) in
  let str = String.sub (desc) (close+1) (length - close - 1) in
  match str with
  | "V" -> 0
  | _ -> 1
;;

let countArgs_from_desc (desc:string) =
  let close = String.index (desc) (')') in
  let str = String.sub (desc) (1) (close - 1) in
  match (String.length (str)) = 0 with
    false -> (countArgs (str) (String.length (str)) (0))
  | true -> (0);;

let get_int_from_hex_char (c:char) =
  match c with
    '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' -> 10
  | 'a' -> 10
  | 'B' -> 11
  | 'b' -> 11
  | 'C' -> 12
  | 'c' -> 12
  | 'D' -> 13
  | 'd' -> 13
  | 'E' -> 14
  | 'e' -> 14
  | 'F' -> 15
  | 'f' -> 15
  | _ -> raise (Invalid_argument "Bogus Hex value");;

let get_hex_string_from_int (input:int) =
  match input with
    0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8"
  | 9 -> "9"
  | 10 -> "A"
  | 11 -> "B"
  | 12 -> "C"
  | 13 -> "D"
  | 14 -> "E"
  | 15 -> "F"
  | _ -> raise (Invalid_argument "Bogus Hex value");;

let rec hex_string_to_int_rec (input:string) (i:int) (pow:int) (res:int) =
  if (i >= 0) then
    hex_string_to_int_rec (input) (i-1) (pow * 16) (res + (get_int_from_hex_char (String.get input i) * pow))
  else
    (res)

let hex_string_to_int (input:string) =
  hex_string_to_int_rec (input) ((String.length input) - 1) (1) (0)

let rec int_to_hex_string_rec (input:int) (res:string) =
  match input with
    0 -> (if (String.length res >= 2) then (res) else ("0" ^ (res)) )
  | _ -> int_to_hex_string_rec (input / 16) ((get_hex_string_from_int (input mod 16)) ^ res)

let int_to_hex_string (input:int) =
  int_to_hex_string_rec (input) ("");;
