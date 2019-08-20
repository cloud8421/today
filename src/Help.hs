module Help where

progDesc :: String
progDesc = "TODAY - CLI Task Manager"

progHeader :: String
progHeader =
  unlines [progDesc, ":", "Efficiently manage tasks from the command line"]

taskfile :: String
taskfile =
  "The path of the Taskfile to use. Can be replaced by a TASKFILE environment variable, which takes precedence over the option."

contextFilter :: String
contextFilter = "The context for the task to filter on, e.g. work or home."

contextMove :: String
contextMove = "The context the task should be moved to, e.g. work or home."

includeContextFilter :: String
includeContextFilter =
  "Which context to include, e.g. work or home. If not provided, it will default to all tasks."

excludeContextFilter :: String
excludeContextFilter =
  "Which context to exclude, e.g. work or home. If not provided, it will default to all tasks."

listTasks :: String
listTasks =
  "List all tasks. Can filter by context with --include-context/-i or --exclude-context/-e)."

addTask :: String
addTask =
  "Add a new task. Requires a text description and optionally takes a context option (--context or -c) to add the task under that context."

taskText :: String
taskText =
  "The text of the task. Use double quotes for special characters. Can include references in the shape of SERVICE#IDENTIFIER, e.g. TODAY#123."

taskId :: String
taskId = "The ID of the task the action should be applied on."

removeTask :: String
removeTask = "Removes an existing task by its ID."

finishTask :: String
finishTask = "Finishes a existing task by its ID, marking it as done."

cancelTask :: String
cancelTask = "Cancel an existing task by its ID."

startTask :: String
startTask = "Start an existing task by its ID."

pauseTask :: String
pauseTask = "Pause an existing task by its ID."

updateTask :: String
updateTask = "Update an existing task with a new text description."

moveTask :: String
moveTask = "Move an existing task under a new context."

clearTasks :: String
clearTasks =
  "Clear all done and cancelled tasks, permanently deleting them from the Taskfile."

inForToday :: String
inForToday =
  "Generate a today message summary, including all pending and in progress tasks. Can filter by context with --include-context/-i or --exclude-context/-e)."

outForToday :: String
outForToday =
  "Generate a out for today message summary, including all in progress, done and cancelled tasks. Can filter by context with --include-context/-i or --exclude-context/-e)."

listRefs :: String
listRefs = "List all configured ref replacement rules."

addRef :: String
addRef = "Add a new ref replacement rule"

deleteRef :: String
deleteRef = "Delete an existing ref replacement rule by its repo."

refService :: String
refService =
  "The service name to use inside tasks. If set to CORE, allows resolving refs in the shape of CORE#<number> in any task description."

refUrlTemplate :: String
refUrlTemplate =
  "The url template to use when expanding a ref in a task description. Should contain the token '$id', which would be interpolated with the reference number."

refRepoAction :: String
refRepoAction = "The ref replacement rule repo the action should be applied on."
