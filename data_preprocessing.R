# =========================================================================
# Load Data ---------------------------------------------------------------

# Set source path and working directory
# Note: change to the directory where you save the .csv file 
rm(list = ls())
source_path = "E:/Stanford/2.2 Year 2 Winter/MS&E 263 Healthcare Management/Case/Case 1 MGH Neurosciences/MSE_235-Case_Study_1/"
setwd(source_path)

# Import the dataset
data = read.csv("raw_data.csv", stringsAsFactors = FALSE)
str(data)

# =========================================================================
# Convert Variable Types --------------------------------------------------

# Convert to "Date" variables
library(zoo)
#check the csv file to see if the date format matches
data$Patient_Ready = strptime(data$Patient_Ready, format = "%m/%d/%Y %H:%M:%S")
data$Bed_Request = strptime(data$Bed_Request, format = "%m/%d/%Y %H:%M:%S")
data$Bed_Assignment = strptime(data$Bed_Assignment, format = "%m/%d/%Y %H:%M:%S")
data$Transfer = strptime(data$Transfer, format = "%m/%d/%Y %H:%M:%S")
data$Time_Out_of_Unit = strptime(data$Time_Out_of_Unit, format = "%m/%d/%Y %H:%M:%S")

# Convert to factor variables
colChr = sapply(data, is.character)
data[, colChr] = lapply(data[, colChr], as.factor)

# =========================================================================
# Re-bucket Categorical Variables -----------------------------------------

# Display all levels
colFactor = sapply(data, is.factor)
sapply(data[, colFactor], levels)

# Re-bucket "Others" group
library(plyr)
data$Location = mapvalues(data$Location, from = c("Other Floor", "Other ICU", "Other PACU", 
                                                  "Other PACU 2", "Floor", "Front Door"), 
                          to = c("Other", "Other", "Other", "Other", "Other", "Other"))


# Drop patients whose origin (Location) is "Other"
data = data[data$Location != "Other",]

# =========================================================================
# Generate New Data Columns -----------------------------------------------

# Calculate durations 
duration = function (time_earlier, time_later) {
  library(zoo)
  t = as.double(difftime(time_later, time_earlier, units = "hours"))
  return(t)
}

#data$Time.Patient.Wait = duration(data$Patient_Ready, data$Transfer)
#data$Time.Bed.Empty = duration(data$Bed_Assignment, data$Transfer) # duration for which the assigned bed is held empty
#data$Time.Patient.Stay = duration(data$Transfer, data$Time_Out_of_Unit)

data$request2assignment = duration(data$Bed_Request, data$Bed_Assignment)
data$assignment2transfer = duration(data$Bed_Assignment, data$Transfer)
data$transfer2out = duration(data$Transfer, data$Time_Out_of_Unit)
data$ready2transfer = duration(data$Patient_Ready, data$Transfer)


# Define paths
data[, "Path"] = paste(data$Location, "->", data$Unit)
data$Path = as.factor(data$Path)
prop.table(table(data$Path))

# Frequency table
library(gmodels)
CrossTable(data$Location, data$Unit, prop.t = TRUE, prop.chisq = FALSE)


# # Decompose Dates 
# # Patient_Ready
# data$Patient_Ready.month = format(data$Patient_Ready, "%m")
# data$Patient_Ready.date = format(data$Patient_Ready, "%Y-%m-%d")
# data$Patient_Ready.day = format(data$Patient_Ready, "%a")
data$Patient_Ready.hour = format(data$Patient_Ready, "%H")



# =========================================================================
# Identify Errorneous Data Entries ----------------------------------------

# Find erroneous data (Duration < 0)
time.vars = c("request2assignment", "assignment2transfer", "transfer2out","ready2transfer")
error.vector = c("error.request2assignment", "error.assignment2transfer", "error.transfer2out","error.ready2transfer")
numCol = ncol(data)
data$error.request2assignment = rep(0,nrow(data))
data$error.assignment2transfer = rep(0,nrow(data))
data$error.transfer2out = rep(0,nrow(data))
data$error.ready2transfer = rep(0,nrow(data))
for (i in 1:4 ) {
  data[which(data[, time.vars[i]] < 0 ), error.vector[i]] = 1
}

sum(data$error.request2assignment)
sum(data$error.assignment2transfer)
sum(data$error.transfer2out)
sum(data$error.ready2transfer)





# Plot histogram 
par(mfrow = c(1,3))
for (i in c(11:13)) {
  hist(data[, i], main = paste(colnames(data[i])),  xlab = paste(colnames(data[i]), "(hr)"))
}


# TODO:=========================================================================
# Calculate hourly demand at each location --------------------------------


# Plot hourly trends by path ----------------------------------------------
library(ggplot2)
library(digest)

# Influx to ICU
ggplot(data[data$Unit == "ICU", ], aes(x = Patient_Ready.hour, fill = Location)) +
  geom_bar(position = "dodge", alpha = 0.5) + 
  ggtitle("Hourly Transfer Demand to ICU") +
  labs(x = "Hour", y = "Demand", fill = "Start Location")

# Influx to Floor
ggplot(data[data$Unit == "Floor", ], aes(x = Patient_Ready.hour, fill = Location)) +
  geom_bar(position = "dodge", alpha = 0.5) + 
  ggtitle("Hourly Transfer Demand to Floor") +
  labs(x = "Hour", y = "Demand", fill = "Start Location")


# Export data -------------------------------------------------------------
write.csv(data, "data_processed.csv")

