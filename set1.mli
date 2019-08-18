(* CRYPTOPALS CHALLENGES SET 1 INTERFACE *)

(** Create a bytestring from a hex-encoded input string *)
val from_hex : string -> bytes

(** RFC 4684 compliant base64 encoding/decoding *)
val to_base64 : bytes -> string

(** RFC 4684 compliant base64url encoding/decoding *)
val to_base64url : bytes -> string

