open Core.Std
let test_http_request = "GET /path/file.html HTTP/1.0\r\nFrom: someuser@jmarshall.com\r\nUser-Agent: HTTPTool/1.0\r\n"

let simple_http_request = "GET /path/file.html\r\n"

let rec explode s =
    match s with
    | "" -> []
    | s' -> (String.get s' 0) :: explode (String.sub s' ~pos:1 ~len:((String.length s') - 1))

let implode l =
    List.map ~f:Char.escaped l
    |> String.concat

exception Out_of_bound_string_index of string

let rec split_helper current_list current_str current_matched_str pattern_list index s_list =
    let current_patt_char =
        match List.nth pattern_list index with
        | Some ch -> ch
        | None -> raise (Out_of_bound_string_index "'index' is outside of pattern_list")
    in
    let is_checking_last_patt_char =
        index = (List.length pattern_list) - 1
    in
    match s_list with
    | [] -> List.rev ((current_str ^ current_matched_str) :: current_list)
    | c::s_list' ->
        if List.hd current_list = Some "" then begin
            List.rev ((implode s_list) :: ((current_str ^ current_matched_str) :: current_list))
        end else begin
            if c = current_patt_char then begin
                if is_checking_last_patt_char then begin
                    split_helper (current_str::current_list) "" "" pattern_list 0 s_list'
                end else begin
                    split_helper current_list current_str (current_matched_str ^ (Char.escaped c)) pattern_list (index + 1) s_list'
                end
            end else begin
                split_helper current_list (current_str ^ (current_matched_str ^ (Char.escaped c))) "" pattern_list 0 s_list'
            end
        end


let split_on_string s pattern =
    let patter_char_list = explode pattern in
    let s_char_list = explode s in
    split_helper [] "" "" patter_char_list 0 s_char_list

type http_method_t =
    | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT | OPTIONS

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
    | "PUT" -> Some PUT
    | "DELETE" -> Some DELETE
    | "TRACE" -> Some TRACE
    | "CONNECT" -> Some CONNECT
    | "OPTIONS" -> Some OPTIONS
    | _ -> raise (Invalid_http_request_method "Can't be recognized")

let get_method_string_from_http_method m =
    match m with
    | GET -> "GET"
    | HEAD -> "HEAD"
    | POST -> "POST"
    | PUT -> "PUT"
    | DELETE -> "DELETE"
    | TRACE -> "TRACE"
    | CONNECT -> "CONNECT"
    | OPTIONS -> "OPTIONS"

let split_http_request_line s =
    let splitted_request_line = String.split ~on:' ' s in
    List.filter splitted_request_line ~f:(fun s -> s <> "")

type http_request_header =
    {
        header: string;
        value: string;
    }

exception Invalid_http_request_header of string

let get_http_request_header header_line =
    let splitted_header_line = String.split header_line ~on:':' in
    let header_option = List.hd splitted_header_line in
    let value_option = List.hd (List.rev splitted_header_line) in
    match header_option, value_option with
    | None, _ -> raise (Invalid_http_request_header "No header attached")
    | _, None -> raise (Invalid_http_request_header "No value attached")
    | Some h, Some v ->
        {
            header = String.strip h;
            value  = String.strip v;
        }


let get_http_request_line l =
    let request_line_tokens = split_http_request_line (List.nth_exn l 0) in
    let request_method = match get_http_method_from_string (List.nth_exn request_line_tokens 0) with
        | Some m -> m
        | None -> raise (Invalid_http_request_method "Can't be recognized")
    in
    let request_uri = List.nth_exn request_line_tokens 1 in
    let request_version =
        match List.nth request_line_tokens 2 with
        | Some token -> token
        | None -> ""
    in
    {
        http_method      = request_method;
        http_request_uri = request_uri;
        http_version     = request_version;
    }

let rec get_http_header_entries_util accumulated_strings remaining_strings =
    match remaining_strings with
    | ""::_ -> List.rev accumulated_strings
    | s::remaining_strings' -> get_http_header_entries_util (s::accumulated_strings) remaining_strings'
    | [] -> []

let rec convert_header_strings_to_records init strings =
    match strings with
    | [] -> List.rev init
    | s::strings' -> convert_header_strings_to_records ((get_http_request_header s)::init) strings'

let get_http_header_entries request_strings =
    let header_and_body_strings_option = List.tl request_strings in
    match header_and_body_strings_option with
    | Some header_and_body_strings ->
        get_http_header_entries_util [] header_and_body_strings
        |> convert_header_strings_to_records []
    | None -> []

let get_http_request_body request_strings =
    match request_strings with
    | ""::s -> List.hd s
    | _ -> None

type http_request =
    {
        request_line: http_request_line;
        headers: http_request_header list;
        body: string option;
    }

let get_http_request request_string =
    let splitted_request_strings = split_on_string request_string "\r\n" in
    {
        request_line = get_http_request_line splitted_request_strings;
        headers      = get_http_header_entries splitted_request_strings;
        body         = get_http_request_body splitted_request_strings;
    }

let print_request request =
    let { request_line = r; headers = h; body = b; } = request in
    let { http_method = r_m; http_request_uri = r_u; http_version = r_v; } = r in
    printf "%s %s %s\n" (get_method_string_from_http_method r_m) r_u r_v;
    List.iter ~f:(fun { header; value } -> printf "%s: %s\n" header value) h;
    match b with
    | Some b_s -> printf "%s" b_s
    | None -> ()

let () =
    [ test_http_request; simple_http_request ]
    |> List.iter ~f:(fun request -> print_request (get_http_request request))

(**
let () =
    let request = get_http_request_line (split_on_string test_http_request "\r\n") in
    printf "method = %s, uri = %s, version = %s\n" (get_method_string_from_http_method request.http_method) request.http_request_uri request.http_version
**)

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
