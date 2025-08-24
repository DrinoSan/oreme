open Task

let example_task () =
  let task =
    create_task 1 "Buy groceries" "Milk, eggs" 1743350400.0 "pending" 3600 Sms
  in
  print_endline (string_of_task task)

let run () = example_task ()
