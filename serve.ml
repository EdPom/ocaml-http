open Core.Std

(* Simple file server *)

let service_base_dir = "http"

type status_code_t = 
    | Ok
    | Created
    | Accepted
    | No_Content
    | Moved_Permanently
    | Moved_Temporarily
    | Not_Modified
    | Bad_Request
    | Unauthorized
    | Forbidden
    | Not_Found
    | Internal_Server_Error
    | Not_Implemented
    | Bad_Gateway
    | Service_Unavailable

let status_code_to_int s =
    match s with
    | Ok                    -> 200
    | Created               -> 201
    | Accepted              -> 202
    | No_Content            -> 204
    | Moved_Permanently     -> 301
    | Moved_Temporarily     -> 302
    | Not_Modified          -> 304
    | Bad_Request           -> 400
    | Unauthorized          -> 401
    | Forbidden             -> 403
    | Not_Found             -> 404
    | Internal_Server_Error -> 500
    | Not_Implemented       -> 501
    | Bad_Gateway           -> 502
    | Service_Unavailable   -> 503

let status_code_to_string s =
    match s with
    | Ok                    -> "Ok"
    | Created               -> "Created"
    | Accepted              -> "Accepted"
    | No_Content            -> "No Content"
    | Moved_Permanently     -> "Moved Permanently"
    | Moved_Temporarily     -> "Moved Temporarily"
    | Not_Modified          -> "Not Modified"
    | Bad_Request           -> "Bad Request"
    | Unauthorized          -> "Unauthorized"
    | Forbidden             -> "Forbidden"
    | Not_Found             -> "Not Found"
    | Internal_Server_Error -> "Internal Server Error"
    | Not_Implemented       -> "Not Implemented"
    | Bad_Gateway           -> "Bad Gateway"
    | Service_Unavailable   -> "Service Unavailable"

type http_status_line =
    {   
        http_version: string;
        status_code: status_code_t;
        reason_phrase: string;
    }


