# =========================================================================
# Load Data ---------------------------------------------------------------

# Set source path and working directory
# Note: change to the directory where you save the .csv file 
rm(list = ls())
source_path = "~/Dropbox/Stanford/2015-2016/2Q/MS&E 235/Case Study 1"
setwd(source_path)

# Import the dataset
data = read.csv("raw_data.csv", stringsAsFactors = FALSE)
str(data)

# =========================================================================
# Convert Variable Types --------------------------------------------------

# Convert to "Date" variables
library(zoo)

data$Patient_Ready = strptime(data$Patient_Ready, format = "%d/%m/%Y %H:%M:%S")
data$Bed_Request = strptime(data$Bed_Request, format = "%d/%m/%Y %H:%M:%S")
data$Bed_Assignment = strptime(data$Bed_Assignment, format = "%d/%m/%Y %H:%M:%S")
data$Transfer = strptime(data$Transfer, format = "%d/%m/%Y %H:%M:%S")
data$Time_Out_of_Unit = strptime(data$Time_Out_of_Unit, format = "%d/%m/%Y %H:%M:%S")

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
  
  #(1) Time.Patient.Wait
  data$Time.Patient.Wait = duration(data$Patient_Ready, data$Transfer)
  
  #(2) Time.Bed.Empty (duration for which the assigned bed is held empty)
  data$Time.Bed.Empty = duration(data$Bed_Assignment, data$Transfer)
  
  #(3) Time.Patient.Stay
  data$Time.Patient.Stay = duration(data$Transfer, data$Time_Out_of_Unit)

# Assign paths
  data[, "Path"] = paste(data$Location, "-", data$Unit)
  data$Path = as.factor(data$Path)


# Decompose Dates 
# Patient_Ready
data$Patient_Ready.month = format(data$Patient_Ready, "%m")
data$Patient_Ready.date = format(data$Patient_Ready, "%Y-%m-%d")
data$Patient_Ready.day = format(data$Patient_Ready, "%a")



# =========================================================================
# Identify Errorneous Data Entries ----------------------------------------

# Find erroneous data (Duration < 0)
time.vars = c("Time.Patient.Wait", "Time.Bed.Empty", "Time.Patient.Stay")
numCol = ncol(data)
for (varname in time.vars) {
  data[which(data[, varname] < 0 ), numCol + 1] = 1
  data[which(data[, varname] >= 0 ), numCol + 1] = 0
}

# Plot histogram 
par(mfrow = c(1,3))
for (i in c(11:13)) {
  hist(data[, i], main = paste(colnames(data[i])),  xlab = paste(colnames(data[i]), "(hr)"))
}


# =========================================================================
# Calculate hourly demand at each location --------------------------------


