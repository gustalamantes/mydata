name <- c("Mandi","Amy","Nicole","Olivia")
distance <- c(0.8,3.1,2.8,4.0)
time <- c(10,30,40,50)
time_in_hours <- time / 60
time_in_hours
speed <- distance / time_in_hours
olivia_time <- time_in_hours[name == "Olivia"]
olivia_time
mandi_speed <- speed[name == "Mandi"]
mandi_speed
faster_runner <-name[which.max(speed)]
faster_runner

