let main () = 
    let byte_string = Bytes.of_string (read_line ()) in
    let base64_string = Set1.to_base64 byte_string in
    print_endline base64_string;
    exit 0;;
main ()
