open Types
open Yojson.Basic.Util

let task_to_json task =
  `Assoc
    [
      ("id", `Int task.id);
      ("title", `String task.title);
      ("description", `String task.description);
      ("due_date", `Float task.due_date);
      ("status", `String task.status);
      ("reminder_offset", `Int task.reminder_offset);
      ( "reminder_type",
        `String
          (match task.reminder_type with
          | Email -> "Email"
          | Sms -> "SMS"
          | Sys_notification -> "Sys_notification") );
    ]

let task_from_json json =
  let id = json |> member "id" |> to_int in
  let title = json |> member "title" |> to_string in
  let description = json |> member "description" |> to_string in
  let due_date = json |> member "due_date" |> to_float in
  let status = json |> member "status" |> to_string in
  let reminder_offset = json |> member "reminder_offset" |> to_int in
  let reminder_type =
    match json |> member "reminder_type" |> to_string with
    | "Email" -> Email
    | "SMS" -> Sms
    | _ -> Sys_notification
  in
  Task.create_task id title description due_date status reminder_offset
    reminder_type

let save_tasks filename tasks =
  let json = `List (List.map task_to_json tasks) in
  Yojson.Basic.to_file filename json

let load_tasks filename =
  try
    let json = Yojson.Basic.from_file filename in
    json |> to_list |> List.map task_from_json
  with _ -> [] (* Seems like the file is empty or some other shit *)
