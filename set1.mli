(* CRYPTOPALS CHALLENGES SET 1 INTERFACE *)

(** Create a bytestring from a hex-encoded input string *)
val from_hex : string -> bytes

(** Create a hex-encoded string from a bytestring input *)
val to_hex : bytes -> string

(** RFC 4684 compliant base64 encoding/decoding *)
val to_base64 : bytes -> string

(** RFC 4684 compliant base64url encoding/decoding *)
val to_base64url : bytes -> string

(** XORs two byte buffers against each other *)
val xor_buffers : bytes -> bytes -> bytes

(** XORs a byte buffer against a single byte *)
val xor_buffer_by_byte : bytes -> char -> bytes

(** A set of recognized words *)
type dictionary

(** Determines the fraction of words in the provided string that are in the
 * provided dictionary *)
val in_dictionary : dictionary -> string -> float

(** Opens a dictionary from a newline-delimiated file *)
val open_dictionary : string -> dictionary
