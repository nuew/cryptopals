(* CRYPTOPALS CHALLENGES SET 1 IMPLEMENTATION *)

let from_hex input =
    (* Converts a hex character into a nibble,
     * or raises an exception if the input isn't a hex character *)
    let nibble_from_hex character = match character with
        '0' -> 0x0 | '1' -> 0x1 | '2' -> 0x2 | '3' -> 0x3 | '4' -> 0x4 | '5' -> 0x5 | '6' -> 0x6 |
        '7' -> 0x7 | '8' -> 0x8 | '9' -> 0x9 | 'a' | 'A' -> 0xa | 'b' | 'B' -> 0xb |
        'c' | 'C' -> 0xc | 'd' | 'D' -> 0xd | 'e' | 'E' -> 0xe | 'f' | 'F' -> 0xf
        | _ -> raise (Invalid_argument "must be between 0x0 and 0xf") in
    let rec bytes_from_hex index acc = 
        if String.length input >= index + 1 then
            (* Get a particular nibble from the input at the specified offset from the index *)
            let nibble num = nibble_from_hex (String.get input (index + num)) lsl (-4 * (num - 1)) in
            let byte = char_of_int (nibble 0 lor nibble 1) in
            Bytes.set acc (index / 2) byte;
            bytes_from_hex (index + 2) acc
        else acc in
    bytes_from_hex 0 (Bytes.create ((String.length input) / 2)) (* expected size of output *)

let to_hex input =
    let hex_from_nibble nibble = match nibble with
        0x0 -> '0' | 0x1 -> '1' | 0x2 -> '2' | 0x3 -> '3' | 0x4 -> '4' | 0x5 -> '5' | 0x6 -> '6' |
        0x7 -> '7' | 0x8 -> '8' | 0x9 -> '9' | 0xa -> 'a' | 0xb -> 'b' | 0xc -> 'c' | 0xd -> 'd' |
        0xe -> 'e' | 0xf -> 'f' | _ -> raise (Invalid_argument "must be between 0x0 and 0xf") in
    let rec hex_from_bytes index acc =
        if Bytes.length input >= index + 1 then
            let byte = int_of_char (Bytes.get input index) in
            let nibbles = [(byte land 0xf0) lsr 4; byte land 0xf] in
            let hex_chars = List.map hex_from_nibble nibbles in
            hex_from_bytes (index + 1) (acc @ hex_chars)
        else String.of_seq (List.to_seq acc) in
    hex_from_bytes 0 []

let base64_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
let base64url_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_="

let to_base64_inner alphabet input =
    let rec to_base64_raw index output =
        (* Get a particular byte from the input at a particular offset from the index
         * and shifted appropriately for that offset to allow a 24-bit structure *)
        let get_byte_in num = 
            if Bytes.length input > index + num then
                Some (int_of_char (Bytes.get input (index + num)) lsl (-8 * (num - 2)))
            else None in
        (* Create the 24-bit/3-byte structure, with two bits to remember the number
         * of initialized bytes *)
        let structure =
            let in_bytes = [get_byte_in 0; get_byte_in 1; get_byte_in 2] in
            let count_somes a b = a + match Option.is_some b with false -> 0 | true -> 1 in
            let init_bytes = List.fold_left count_somes 0 in_bytes in
            let lor_some_or_zero a b = a lor (Option.fold ~none:0 ~some:( ~+ ) b) in
            List.fold_left lor_some_or_zero (init_bytes lsl 24) in_bytes in
        (* Get and base64-encode a 6-bit output group from the structure *)
        let get_byte_out num =
            let initialized = (structure land (0x3 lsl 24)) lsr 24 in
            if initialized >= num then
                let offset = -6 * (num - 3) in
                let mask = 0b111111 lsl offset in
                let byte = (structure land mask) lsr offset in
                String.get alphabet byte
            else String.get alphabet 64 in
        (* Create a bytestream of the newly encoded bytes for output *)
        let new_output = Bytes.of_seq (List.to_seq [get_byte_out 0; get_byte_out 1;
                                                    get_byte_out 2; get_byte_out 3]) in
        (* Concatenate the new output bytestream with the existing one and continue *)
        let output = Bytes.cat output new_output in
        (* If there are more bytes, convert another group. Otherwise, return the output buffer *)
        if Bytes.length input > (index + 3) then to_base64_raw (index + 3) output else output in
    if Bytes.length input <> 0 then (* unless the input is empty *)
        Bytes.to_string (to_base64_raw 0 Bytes.empty) (* convert the input to a base64 group *)
    else "" (* otherwise return the empty string *)

let to_base64 = to_base64_inner base64_alphabet
let to_base64url = to_base64_inner base64url_alphabet

let xor_buffers bufA bufB =
    let mapxor index byteA =
        let byteA, byteB = int_of_char byteA, int_of_char (Bytes.get bufB index) in
        char_of_int (byteA lxor byteB) in
    Bytes.mapi mapxor bufA
