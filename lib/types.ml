type reminder_type = Email | Sms | Sys_notification

type task = {
  id : int;
  title : string;
  description : string;
  due_date : float;
  status : string;
  reminder_offset : int;
  reminder_type : reminder_type;
}
