library(taskscheduleR)

## Minha agenda.
taskscheduler_create(taskname = "GeraControleXML",
                     rscript = "F:\\CTO\\OUTROS\\R\\script.R", 
                     schedule = "DAILY",
                     starttime = "06:00") 

# Ver tarefas agendadas...
x <- taskscheduleR::taskscheduler_ls()

taskscheduleR::taskscheduler_delete("GeraControleXML")
taskscheduleR::taskcheduler_runnow("GeraControleXML")




















myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
# "C:/Users/Marcos/Documents/R/win-library/3.4/taskscheduleR/extdata/helloworld.R"


## run script every day at 09:10
taskscheduler_create(taskname = "myfancyscriptdaily", rscript = myscript, 
                     schedule = "DAILY", starttime = "09:10")


## delete the tasks
taskscheduler_delete(taskname = "myfancyscript")
taskscheduler_delete(taskname = "myfancyscriptdaily")

