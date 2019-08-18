let main () = 
    let byte_string_1 = Set1.from_hex (read_line ()) in
    let byte_string_2 = Set1.from_hex (read_line ()) in
    let xorred_buffer = Set1.xor_buffers byte_string_1 byte_string_2 in
    Printf.printf "%s\n" (Set1.to_hex xorred_buffer);
    exit 0;;
main ()
