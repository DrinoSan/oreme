open Types

let create_task id title description due_date status reminder_offset
    reminder_type =
  { id; title; description; due_date; status; reminder_offset; reminder_type }

let string_of_task task =
  Printf.sprintf
    "Task %d: %s\n\
     Description: %s\n\
     Due: %s\n\
     Status: %s\n\
     Reminder Offset: %d\n\
     Reminder Type: %s"
    task.id task.title task.description
    (Ptime.to_rfc3339 (Ptime.of_float_s task.due_date |> Option.get))
    task.status task.reminder_offset
    (match task.reminder_type with
    | Email -> "Email"
    | Sms -> "Sms"
    | Sys_notification -> "System Notification")
