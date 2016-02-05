# =========================================================================
# Load Data ---------------------------------------------------------------

# Set source path and working directory
# Note: change to the directory where you save the .csv file 
rm(list = ls())

# Eric
# source_path = "E:/Stanford/2.2 Year 2 Winter/MS&E 263 Healthcare Management/Case/Case 1 MGH Neurosciences/MSE_235-Case_Study_1/"

# Haju
source_path = "~/Dropbox/Stanford/2015-2016/2Q/MS&E 235/Case Study 1"

setwd(source_path)

# Import the dataset
data = read.csv("raw_data.csv", stringsAsFactors = FALSE)
str(data)

# =========================================================================
# Convert Variable Types --------------------------------------------------

# Convert to "Date" variables
library(zoo)

#check the csv file to see if the date format matches
colDate = c("Patient_Ready", "Bed_Request", "Bed_Assignment","Transfer", "Time_Out_of_Unit")
for (varname in colDate) {
  data[[varname]] = strptime(data[[varname]], format = "%m/%d/%y %H:%M")
}

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
# Location to Unit
data[, "Path"] = paste(data$Location, "->", data$Unit)
data$Path = as.factor(data$Path)
prop.table(table(data$Path))

# Location to Floor
data[, "Path_Floor"] = paste(data$Location, "->", data$Floor_Number)
data$Path_Floor = as.factor(data$Path_Floor)
prop.table(table(data$Path_Floor))

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
colTime = c("request2assignment", "assignment2transfer", "transfer2out","ready2transfer")
error.vector = c("error.request2assignment", "error.assignment2transfer", "error.transfer2out","error.ready2transfer")
numCol = ncol(data)
data$error.request2assignment = rep(0,nrow(data))
data$error.assignment2transfer = rep(0,nrow(data))
data$error.transfer2out = rep(0,nrow(data))
data$error.ready2transfer = rep(0,nrow(data))
for (i in 1:4 ) {
  data[which(data[, colTime[i]] < 0 ), error.vector[i]] = 1
}

sum(data$error.request2assignment) # 684 errors (bed assignment before request)
sum(data$error.assignment2transfer)
sum(data$error.transfer2out)
sum(data$error.ready2transfer)


# Analyze the ones with negative request2assignment
data_neg = data[data$request2assignment < 0, ]
prop.table(table(data_neg$Path))
prop.table(table(data_neg$Location))
prop.table(table(data_neg$Unit))


# Plot histogram 
par(mfrow = c(2,2))
for (i in c(11:14)) {
  hist(data[, i], main = paste(colnames(data[i])),  xlab = paste(colnames(data[i]), "(hr)"))
}
# Note: request2assignment error also visible in the histogram


# Identify outliers by box plots
library(ggplot2)
library(gridExtra)

# Box Plot (Location / Unit)
plots = lapply(colTime, function(x)
  ggplot(data, aes(x = Location, y = data[, x], fill = Unit)) +
    geom_boxplot() +
    ggtitle(paste("Box Plot Comparison of ", x, " time")) +
    labs(x = "Start Location", y = paste(x, "Time"), fill = "Unit") +
    coord_flip()
)

do.call(grid.arrange, plots)

plots_f = lapply(colTime, function(x)
  ggplot(data, aes(x = Location, y = data[, x], fill = Floor_Number)) +
    geom_boxplot() +
    ggtitle(paste("Box Plot Comparison of ", x, " time")) +
    labs(x = "Start Location", y = paste(x, "Time"), fill = "Floor_Number") +
    coord_flip()
)

do.call(grid.arrange, plots_f)

# # Floor
# ggplot(data[data$Unit == "Floor", ], aes(x = Location, y = ready2transfer, fill = Location)) +
#   geom_boxplot() +
#   ggtitle("Box Plot Comparison of Patient Wait Time at Floor") +
#   labs(x = "Start Location", y = "ready2transfer", fill = "Starting Location")
# 
# # ICU 
# ggplot(data[data$Unit == "ICU", ], aes(x = Location, y = ready2transfer, fill = Location)) +
#   geom_boxplot() +
#   ggtitle("Box Plot Comparison of Patient Wait Time ICU") +
#   labs(x = "Start Location", y = "ready2transfer", fill = "Start Location")
#   
# # Both
# p1 = ggplot(data, aes(x = Path, y = data[[varname]], fill = Location)) +
#   geom_boxplot(outlier.shape = "o") +
#   ggtitle(paste("Box Plot Comparison of ", varname, " time")) +
#   labs(x = "Unit", y = paste(varname), fill = "Start Location")
# 
# p3 = ggplot(data, aes(x = Location, y = data[[varname]], fill = Unit)) +
#   geom_boxplot() +
#   ggtitle(paste("Box Plot Comparison of ", varname, " time")) +
#   labs(x = "Start Location", y = paste(varname), fill = "Unit")

# TODO:=========================================================================
# Calculate hourly demand at each location --------------------------------

# Plot hourly trends by path ----------------------------------------------
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

# 
# Calculate the occupancy at each location --------------------------------
library(hillmakeR)
library(plyr)
library(doBy)

# Determine how many patients are at each given location each hour
listFloor = c("L06", "L07", "L08")

for (floorName in listFloor) {
  data_sub = data[data$Floor_Number == floorName, ]
  
  occupancy_temp = occupancy(startTimes = as.POSIXct(data_sub$Transfer), 
                             stopTimes = as.POSIXct(data_sub$Time_Out_of_Unit), 
                             resolution = "hour")
  
  occupancy_temp$hour = as.POSIXlt(occupancy_temp$times)$hour
  
  byHourOfDay = ddply(occupancy_temp, c("hour"),
                      function(x) c(min = min(x$counts),
                                    mean = mean(x$counts),
                                    median = median(x$counts),
                                    q90 = quantile(x$counts, 0.9, names = FALSE),
                                    max = max(x$counts)
                      )
  )
  varName = paste0("occupancy_", floorName)
  assign(varName, occupancy)
  varName = paste0("occupancy_hourly_", floorName)
  assign(varName, byHourOfDay)
}

colnames(occupancy_L06)[2] = "L6_Counts"
colnames(occupancy_L07)[2] = "L7_Counts"
colnames(occupancy_L08)[2] = "L8_Counts"

occupancy = join_all(list(occupancy_L06, occupancy_L07, occupancy_L08), by = "times", type = "full")

# byHourAndFloor = ddply(occupancy, c("hour"),
#                        function(x) {c(min = min(x),
#                                      mean = mean(x),
#                                      median = median(x),
#                                      q90 = quantile(x, 0.9, names = FALSE),
#                                      max = max(x)
#                                      )
# )

# Determine how many patietns are at hospital by hour of day
patientCount = occupancy(startTimes = as.POSIXct(data$Transfer), stopTimes = as.POSIXct(data$Time_Out_of_Unit), resolution = "hour")
patientCount$hour = as.POSIXlt(patientCount$times)$hour
byHourOfDay = ddply(patientCount, c("hour"),
                    function(x) c(min = min(x$counts),
                                  mean = mean(x$counts),
                                  median = median(x$counts),
                                  q90 = quantile(x$counts, 0.9, names = FALSE),
                                  max = max(x$counts)
                                  )
                    )

# Display the output graphically
# 
# plot(byHourOfDay$mean, type = "o")

ggplot(byHourOfDay, aes(x = hour, y = mean)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  xlim(0, 23) +
  scale_x_continuous(breaks = 0:23) 
  


# 
# library(lubridate)
# head(ymd_hms(data$Patient_Ready.hour))
# # The occupancy: (# Transfers into that unit before that date-time) minus the total number of Departures (Time out of unit)s. Here are the steps to calculate this.
# 
# # Create a subset of the data that includes only those lines where patients transferred into the unit of interest (Filter in Excel or the subset() command in R)
# #format(data$Patient_Ready, format="%m/%d/%y %H:00")
# 
# # Determine when you want the occupancy on the given day (noon, 3pm, midnight, etc.) for example noon would be 02/01/2012 12:00:00 - if you are working in R, I recommend the Lubridate package that you've seen in the R scripts I've provided
# data_occupancy = data.frame(seq(from = as.POSIXct("2012-01-01 00:00"), to = as.POSIXct("2013-07-01 00:00"), by = "hour"))
# colnames(data_occupancy)[1] = "Time"
# 
# # Add up the number of lines where the transfer time is less than 02/01/2012 12:00:00 - call this X
# 

# 
# # Add up the number of lines where the Out of Unit Time is less than 02/01/2012 12:00:00 - call this Y
# 
# # Occupancy =  X - Y (except for the first couple of weeks of the data set - think about why).
# 
# # Create a new object (column in Excel or vector in R) with one entry for each date/time of interest and repeat steps 1 - 5 for each entry in this object.
# # 

# Export data -------------------------------------------------------------
write.csv(data, "data_processed.csv")



## TEST
# Clear workspace
rm(list = ls(all.names = TRUE))

# Set source path, working directory, and filename
source_path = "~/Google Drive/W16-MS&E-235/Homework 3"
setwd(source_path)
rm(source_path)

# Import the csv file and save into a data frame
df.train = read.csv("churn_train.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "")
df.test = read.csv("churn_test.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "")

head(df.train, 5)

# Remove the first column (Customer ID) from the data frame
df.train = df.train[, 2:ncol(df.train)]
df.test = df.test[, 2:ncol(df.test)]

# Check variable types
sapply(df.train, class)

# Apply SVM
## (1) Linear Kernel
library(caret)
svm.fitControl = trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

svmLinearGrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
svmLinearTune = train(LEAVE ~ ., data = df.train, method = "svmLinear", verbose = TRUE, metric = "ROC", 
                      tuneGrid = svmLinearGrid, trControl = svm.fitControl)


## (2) Polynomial Kernel
svmPolyGrid = expand.grid(degree = c(2, 3), scale = c(0.001, 0.01, 0.1, 1, 10, 100), C = c(0.001, 0.01, 0.1, 1, 10, 100))
svmPolyTune = train(LEAVE ~ ., data = df.train, method = "svmPoly", verbose = TRUE, metric = "ROC",
                    tuneGrid = svmPolyGrid, trControl = svm.fitControl)
##########HERE
# Clear workspace
rm(list = ls(all.names = TRUE))

# Set source path, working directory, and filename
source_path = "~/Google Drive/W16-MS&E-235/Homework 3"
setwd(source_path)
rm(source_path)

# Import the csv file and save into a data frame
df.train = read.csv("churn_train.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "")
df.test = read.csv("churn_test.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "")

head(df.train, 5)

# Remove the first column (Customer ID) from the data frame
df.train = df.train[, -1]
df.test = df.test[, -1]

# Check variable types
sapply(df.train, class)

# Apply SVMl
## (1) Radial Kernel
library(e1071)
library(caret)
attach(df.train)
svm_tune = tune(method = "svm", LEAVE ~ ., data = df.train,
                kernel = "radial", ranges = list(cost = 10^(-1:2), gamma = c(0.5, 1, 2)))
                
model.svm = svm(LEAVE ~ ., data = df.train)

pred.svm = predict(model.svm, df.test)
t = confusionMatrix(data = pred.svm, reference = df.test$LEAVE)
200*(t$table[1,1] + t$table[1,2]) + 1000*(t$table[2,1]) #1106400 BAD :(

## (2) Linear Kernel
library(kernlab)

svm.fitControl = trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

svmLinearGrid = expand.grid(C = c(0.001, 0.01))
# , 0.1, 1, 5, 10, 100
svmLinearTune = train(LEAVE ~ ., data = df.train, method = "svmLinear", trace = TRUE, metric = "ROC",
                      tuneGrid = svmLinearGrid, trControl = svm.fitControl)

pred.svm.linear = predict(svmLinearTune, df.test)
t = confusionMatrix(data = pred.svm.linear, reference = df.test$LEAVE)
200*(t$table[1,1] + t$table[1,2]) + 1000*(t$table[2,1]) #1194400 BAD :(

## (3) Polynomial Kernel
svmPolyGrid = expand.grid(degree = c(2, 3), scale = c(0.001, 0.01, 0.1, 1, 10, 100), C = c(0.001, 0.01, 0.1, 1, 10, 100))
svmPolyTune = train(LEAVE ~ ., data = df.train, method = "svmPoly", trace = TRUE, metric = "ROC",
                    tuneGrid = svmPolyGrid, trControl = svm.fitControl)


# C50 Classification Tree (Best so far)
library(C50)

costs = matrix(c(200, 200, 1000, 0) , 2)
colnames(costs) = rownames(costs) = c("LEAVE", "STAY")
costs

q3_c50 = C5.0(q3.df.train[, -12], q3.df.train$LEAVE, trials = 100, costs = costs)
df_pred = predict(q3_c50, q3.df.test)

t = confusionMatrix(df_pred, q3.df.test$LEAVE)

200*(t$table[1,1] + t$table[1,2]) + 1000*(t$table[2,1]) # $740,600



