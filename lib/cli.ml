open Cmdliner
open Types
open Task
open Storage

let add_task_cmd tasks_file id title description due_date reminder_offset
    reminder_type =
  let tasks = load_tasks tasks_file in
  let reminder_type =
    match reminder_type with
    | "Email" -> Email
    | "SMS" -> Sms
    | _ -> Sys_notification
  in
  let new_task =
    create_task id title description due_date "pendig" reminder_offset
      reminder_type
  in
  let new_tasks = new_task :: tasks in
  save_tasks tasks_file new_tasks;
  Printf.printf "Added task: %s\n" title

let list_tasks_cmd tasks_file =
  let tasks = load_tasks tasks_file in
  List.iter (fun task -> print_endline (string_of_task task)) tasks

let list_cmd =
  let tasks_file =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TASKS_FILE")
  in
  ( Term.(const list_tasks_cmd $ tasks_file),
    Cmd.info "list" ~doc:"List all tasks" )

let remind_cmd tasks_file =
  let tasks = load_tasks tasks_file in
  let now = Unix.gettimeofday () in
  let reminders =
    List.filter
      (fun task ->
        let reminder_time =
          task.due_date -. float_of_int task.reminder_offset
        in
        task.status = "pending" && now >= reminder_time && now <= task.due_date)
      tasks
  in
  List.iter
    (fun task ->
      print_endline ("Reminder: " ^ string_of_task task);
      match task.reminder_type with
      | Email -> ()
      | Sms -> ()
      | Sys_notification ->
          ignore
            (Sys.command
               ("notify-send 'Task Reminder' '" ^ string_of_task task ^ "'")))
    reminders

let add_cmd = 
  let id = Arg.(required & pos 0 (some int) None & info [] ~docv:"ID") in
(*   let id = Arg.required & Arg.pos 0 (Arg.some Arg.int) None & Arg.info [] ~docv:"ID"   <-- Could also be written like this but above line uses open scoping*)
  let title = Arg.(required & pos 1 (some string) None & info [] ~docv:"TITLE") in
  let description = Arg.(required & pos 2 (some string) None & info [] ~docv:"DESCRIPTION") in




