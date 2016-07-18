open Core.Std
let test_http_request = "GET /path/file.html HTTP/1.0\r\nFrom: someuser@jmarshall.com\r\nUser-Agent: HTTPTool/1.0\r\n"

let simple_http_request = "GET /path/file.html\r\n"

let rec explode s =
    match s with
    | "" -> []
    | s' -> (String.get s' 0) :: explode (String.sub s' ~pos:1 ~len:((String.length s') - 1))

let implode l =
    String.concat l

let rec split_helper current_list current_str current_matched_str pattern_list index s_list =
    let current_patt_char =
        match List.nth pattern_list index with
        | Some c -> c
        | None -> '0'
    in
    let is_checking_last_patt_char =
        index = (List.length pattern_list) - 1
    in
    match s_list with
    | [] -> List.rev ((current_str ^ current_matched_str) :: current_list)
    | c::s_list' ->
        if c = current_patt_char then (
            if is_checking_last_patt_char then (
                split_helper (current_str::current_list) "" "" pattern_list 0 s_list'
            ) else (
                split_helper current_list current_str (current_matched_str ^ (Char.escaped c)) pattern_list (index + 1) s_list'
            )
        ) else (
            split_helper current_list (current_str ^ (current_matched_str ^ (Char.escaped c))) "" pattern_list 0 s_list'
        )


let split_on_string s pattern =
    let patter_char_list = explode pattern in
    let s_char_list = explode s in
    split_helper [] "" "" patter_char_list 0 s_char_list

type http_method_t =
    | GET | HEAD | POST

type http_request_line =
    {
        http_method: http_method_t;
        http_request_uri: string;
        http_version: string;
    }

exception Invalid_http_request_method of string

let get_http_method_from_string s =
    match s with
    | "GET" -> Some GET
    | "HEAD" -> Some HEAD
    | "POST" -> Some POST
    | _ -> raise (Invalid_http_request_method "Can't be recognized")

let get_method_string_from_http_method m =
    match m with
    | GET -> "GET"
    | HEAD -> "HEAD"
    | POST -> "POST"

let split_http_request_line s =
    let splitted_request_line = String.split ~on:' ' s in
    List.filter splitted_request_line ~f:(fun s -> s <> "")


let get_http_request_line l =
    let request_line_tokens = split_http_request_line (List.nth_exn l 0) in
    let request_method = match get_http_method_from_string (List.nth_exn request_line_tokens 0) with
        | Some m -> m
        | None -> raise (Invalid_http_request_method "Can't be recognized")
    in
    let request_uri = List.nth_exn request_line_tokens 1 in
    let request_version = List.nth_exn request_line_tokens 2 in
    {
        http_method      = request_method;
        http_request_uri = request_uri;
        http_version     = request_version;
    }

let () =
    let request = get_http_request_line (split_on_string test_http_request "\r\n") in
    printf "method = %s, uri = %s, version = %s\n" (get_method_string_from_http_method request.http_method) request.http_request_uri request.http_version

(**
let () =
    split_on_string test_http_request "\r\n"
    |> List.iter ~f:(fun s -> printf "Line: %s\n" s);
    split_on_string simple_http_request "\r\n"
    |> List.iter ~f:(fun s -> printf "Line: %s\n" s)
*)
(**
let get_header_string s =
    (** let l = String.split_on_chars ~on:['\r';'\n'] s in *)
    let l = Regex.split (Str.regexp "\r\n") s in
    List.hd l
*)
(**
let () =
    let s = get_header_string test_http_request in
    match s with
    | None -> printf "None!\n"
    | Some s' -> printf "Header = %s\n" s'
*)
