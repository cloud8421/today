module Help where

progDesc :: String
progDesc = "T - CLI Task Manager"

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

maybeContextFilter :: String
maybeContextFilter =
  "The context for the task to filter on, e.g. work or home. If not provided, it will default to all tasks."

listTasks :: String
listTasks =
  "List all tasks. Optionally takes a context option (--context or -c) to filter tasks by that context."

addTask :: String
addTask =
  "Add a new task. Requires a text description and optionally takes a context option (--context or -c) to add the task under that context."

taskText :: String
taskText =
  "The text of the task. Use double quotes for special characters. Can include references in the shape of REPONAME#ISSUE_NUMBER, e.g. T#123."

taskId :: String
taskId = "The ID of the task the action should be applied on."

deleteTask :: String
deleteTask = "Deletes an existing task by its ID."

checkTask :: String
checkTask = "Check an existing task by its ID, marking it as done."

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

today :: String
today =
  "Generate a today message summary, including all pending and in progress tasks. Optionally takes a context option (--context or -c) to filter tasks by that context."

listRefs :: String
listRefs = "List all configured ref replacement rules."

addRef :: String
addRef = "Add a new ref replacement rule"

deleteRef :: String
deleteRef = "Delete an existing ref replacement rule by its repo."

refRepo :: String
refRepo =
  "The ref name to use inside tasks. If set to CORE, allows resolving refs in the shape of CORE#<number> to any task description."

refPath :: String
refPath =
  "The github path the ref points to. If set to cloud8421/t, it would point to https://github.com/cloud8421/t/issues."

refRepoAction :: String
refRepoAction = "The ref replacement rule repo the action should be applied on."
