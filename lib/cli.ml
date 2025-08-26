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
    create_task id title description due_date "pending" reminder_offset
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
  Cmd.v
    (Cmd.info "list" ~doc:"List all tasks")
    Term.(const list_tasks_cmd $ tasks_file)

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
          let cmd =
            Printf.sprintf
              "osascript -e 'display notification \"Task due: %s\" with title \
               \"Task Reminder\" subtitle \"Due now\" sound name \"Ping\"'"
              task.title
          in
          ignore (Sys.command cmd))
    reminders

let add_cmd =
  let id = Arg.(required & pos 0 (some int) None & info [] ~docv:"ID") in
  let title =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"TITLE")
  in
  let description =
    Arg.(required & pos 2 (some string) None & info [] ~docv:"DESCRIPTION")
  in
  let due_date =
    Arg.(required & pos 3 (some float) None & info [] ~docv:"DUE_DATE")
  in
  let tasks_file =
    Arg.(
      value & opt string "tasks.json"
      & info [ "file" ] ~docv:"TASKS_FILE"
          ~doc:"Tasks file (default: tasks.json)")
  in
  let reminder_offset =
    Arg.(
      value & opt int 3600
      & info [ "remind" ] ~docv:"REMINDER_OFFSET"
          ~doc:"Reminder offset in seconds")
  in
  let reminder_type =
    Arg.(
      value
      & opt string "sys_notification"
      & info [ "type" ] ~docv:"REMINDER_TYPE"
          ~doc:"Reminder type (email, sms, sys_notification)")
  in
  Cmd.v
    (Cmd.info "add" ~doc:"Add a new task")
    Term.(
      const add_task_cmd $ tasks_file $ id $ title $ description $ due_date
      $ reminder_offset $ reminder_type)

let remind_cmd =
  let tasks_file =
    Arg.(
      value & opt string "tasks.json"
      & info [ "file" ] ~docv:"TASKS_FILE"
          ~doc:"Tasks file (default: tasks.json)")
  in
  Cmd.v
    (Cmd.info "remind" ~doc:"Show task reminders")
    Term.(const remind_cmd $ tasks_file)

let default_cmd = Cmd.info "oreme" ~doc:"Task manager CLI"

let run () =
  let default =
    Cmd.v default_cmd
      Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ()))
  in
  let cmd = Cmd.group default_cmd [ default; add_cmd; list_cmd; remind_cmd ] in
  Stdlib.exit (Cmd.eval cmd)
