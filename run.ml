let main () = 
    let byte_string = Set1.from_hex (read_line ()) in
    let dictionary = Set1.open_dictionary "/usr/share/dict/words" in
    let rec test_char acc byte =
        let attempt = Set1.xor_buffer_by_byte byte_string (char_of_int byte) in
        let attempt = Bytes.to_string attempt in
        let score = Set1.in_dictionary dictionary attempt in
        if score > Float.epsilon then Printf.printf "0x%02x\t%.4f\t%S\n" byte score attempt;
        if byte < 255 then
            test_char ((score, attempt) :: acc) (byte + 1)
        else acc in
    let _ = test_char [] 0 in
    exit 0;;
main ()
