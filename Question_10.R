library(dplyr)
library(factoextra)
library(ggplot2)
library(ggpubr)
library(moments)
library(chron)
#Copying the original data to a new M1_data dataset
M1_speed_data$Date<-as.Date(M1_speed_data$Date, format="%d-%m-%Y")
M1_speed_data$Time <- times(format(M1_speed_data$Time, nsmall = 2, width = 6))
M1_data<-M1_speed_data
#Formatting the Time column
#M1_data$Time <- as.POSIXct(M1_data$Time, format = "%Y-%m-%d %H:%M:%S")
View(M1_data)
#changing the names of the Information_N and Information_S columns
names(M1_data)[8]<-'Road_condition_north'
names(M1_data)[13]<-'Road_condition_south'

#MACRO LEVEL ANALYSIS ON OVERALL SPEED

#Calculating base statistic values 
#North
average_speed_n<-mean(M1_data$Speed_N)
average_speed_n
stdev_n<-sd(M1_data$Speed_N)
stdev_n
range_speed_n<-range(M1_data$Speed_N)
range_speed_n
quantile_speed_n<-quantile(M1_data$Speed_N)
quantile_speed_n
#South
average_speed_s<-mean(M1_data$Speed_S)
average_speed_s
stdev_s<-sd(M1_data$Speed_S)
stdev_s
range_speed_s<-range(M1_data$Speed_S)
range_speed_s
quantile_speed_s<-quantile(M1_data$Speed_S)
quantile_speed_s

# Create a density plot of the Speed_N and Speed_S variable 
#Both these plots show that most of the speeds fall between 60 and 70
#with some outlines below that range
ggplot(M1_data, aes(x = Speed_N)) +
  geom_density() +
  labs(x = "Speed Northbound (mph)", y = "Density")

ggplot(M1_data, aes(x = Speed_S)) +
  geom_density() +
  labs(x = "Speed Southbount (mph)", y = "Density")
#Checking for type of distribution of the overall south and north speed
hist(M1_data$Speed_N, xlab = 'Speed (mph)', main = "Northbound Speed") #Negatively skewed 
hist(M1_data$Speed_S, xlab = 'Speed (mph)', main = "Southbound Speed") #Negatively skewed
#Plotting the speed quantile (Package ggpubr)
ggqqplot(M1_data$Speed_N, main = "Normal QQ Plot Northbound Speed") #Confirms heavy left Skewness
ggqqplot(M1_data$Speed_S, main = "Normal QQ Plot Southbound Speed") #Confirms heavy left Skewness
skewness(M1_data$Speed_N) #Use package "moments"
skewness(M1_data$Speed_S) #Use package "moments"

#Creating a bar chart of both north and south speeds
counts_n<-table(M1_data$Speed_N)
barplot(counts_n, main = "Northbound Speed", xlab = 'Speed (mph)', ylab = "Count") #Shows a mode of 70mph

counts_s<-table(M1_data$Speed_S)
barplot(counts_s, main = "Southbound Speed", xlab = 'Speed (mph)', ylab = "Count") #Shows a mode of 70mph

#Scatter plot between speed and junction
ggplot(M1_data, aes(x = Speed_N, y = Junction_N)) + 
  geom_point() +
  xlab("Speed_N") + ylab("Junction_N") +
  ggtitle("Relationship between Speed_N and Junction_N")

ggplot(M1_data, aes(x = Speed_S, y = Junction_S)) + 
  geom_point() +
  xlab("Speed_S") + ylab("Junction_S") +
  ggtitle("Relationship between Speed_S and Junction_S")

#Creating a box plot of the speed values for each day of the week 

#Ordering the days of the week from Monday to Sunday
M1_data$Day<-factor(M1_data$Day, levels = c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday"
))

#Box plot for speed vs. days of the week 
boxplot(Speed_N~Day,data=M1_data, 
        main="Comparison of NorthBound Speed Values of Each Day of the Week", 
        xlab="Day", ylab="Northbound Speed (mph)" , 
        col=(c("red", "blue", "yellow", "violet", "orange", "gray", 'brown')))

boxplot(Speed_S~Day,data=M1_data, 
        main="Comparison of Southbound Speed Values of Each Day of the Week", 
        xlab="Day", ylab="Southbound Speed (mph)" , 
        col=(c("red", "blue", "yellow", "violet", "orange", "gray", 'brown')))

#Scattered plot and box plot for Time vs. Speed
#M1_data$Time_2 <- as.POSIXct(M1_data$Time, format="%H:%M:%S")
plot(M1_data$Time, M1_data$Speed_N, 
     main = "Scatter plot Time VS Northbound Speed", 
     xlab = "Time", ylab = "Northbound speed (mph)")
boxplot(Speed_N~Time,data=M1_data, 
        main="Boxplot for Time VS Northbound Speed", 
        xlab="Time", ylab="Northbound Speed (mph)" , 
        col=(c("red", "blue", "yellow")))

plot(M1_data$Time, M1_data$Speed_S, 
     main = "Scatter plot Time VS Southbound Speed", 
     xlab = "Time", ylab = "Southbound speed (mph)")
boxplot(Speed_S~Time,data=M1_data, 
        main="Boxplot for Time VS Southbound Speed", 
        xlab="Time", ylab="Southbound Speed (mph)" , 
        col=(c("red", "blue", "yellow")))

#MICRO LEVEL ANALYSIS ON OVERALL SPEED
#Creating box plots for speeds based on time of day

#Speed data at 10am for all days of the week
#subset_speed_10am <- subset(M1_data, format(Time, "%H:%M:%S") == "10:00:00")
subset_speed_10am <- subset(M1_data, Time == "10:00:00")
#subset_speed_10am<-subset(M1_data, Time == M1_data$Time)
View(subset_speed_10am)
mean(subset_speed_10am$Speed_N)
mean(subset_speed_10am$Speed_S)
speed_N_10<-subset_speed_10am$Speed_N
boxplot(Speed_N~Day, data = subset_speed_10am, xlab = "10AM", ylab = 
          "Speed Northbound (mph)", main = "Speed Northbound at 10AM",
        col=(c("red", "blue", "yellow", "violet", "orange", "gray", 'brown')))
speed_S_10<-subset_speed_10am$Speed_S
boxplot(Speed_S~Day, data = subset_speed_10am, xlab = "10AM", ylab = 
          "Speed Southbound (mph)", main = "Speed Southbound at 10AM",
        col=(c("red", "blue", "yellow", "violet", "orange", "gray", 'brown')))
#Speed data at 3pm for all days of the week
#subset_speed_3pm<-subset(M1_data, Time == '1970-01-01 15:00:00')
#subset_speed_3pm <- subset(M1_data, format(Time, "%H:%M:%S") == "15:00:00")
subset_speed_3pm <- subset(M1_data, Time == "15:00:00")
mean(subset_speed_3pm$Speed_N)
mean(subset_speed_3pm$Speed_S)
speed_N_3<-subset_speed_3pm$Speed_N
boxplot(Speed_N~Day, data = subset_speed_3pm, xlab = "3PM", ylab = 
          "Speed Northbound (mph)", main = "Speed Northbound at 3PM",
        col=(c("red", "blue", "yellow", "violet", "orange", "gray", 'brown')))
speed_S_3<-subset_speed_3pm$Speed_S
boxplot(Speed_S~Day, data = subset_speed_3pm, xlab = "3PM", ylab = 
          "Speed Southbound (mph)", main = "Speed Southbound at 3PM",
        col=(c("red", "blue", "yellow", "violet", "orange", "gray", 'brown')))
#Speed data at 7pm for all days of the week
#subset_speed_7pm<-subset(M1_data, Time == '1970-01-01 19:00:00')
#subset_speed_7pm <- subset(M1_data, format(Time, "%H:%M:%S") == "19:00:00")
subset_speed_7pm <- subset(M1_data, Time == "19:00:00")
mean(subset_speed_7pm$Speed_N)
mean(subset_speed_7pm$Speed_S)
speed_N_7<-subset_speed_7pm$Speed_N
boxplot(Speed_N~Day, data = subset_speed_7pm, xlab = "7PM", ylab = 
          "Speed Northbound (mph)", main = "Speed Northbound at 7PM",
        col=(c("red", "blue", "yellow", "violet", "orange", "gray", 'brown')))
speed_S_7<-subset_speed_7pm$Speed_S
boxplot(Speed_S~Day, data = subset_speed_7pm, xlab = "7PM", ylab = 
          "Speed Southbound (mph)", main = "Speed Southbound at 7PM",
        col=(c("red", "blue", "yellow", "violet", "orange", "gray", 'brown')))

#Average Northbound and southbound speed values based on the day of the week
speed_mon<-subset(M1_data, Day == "Monday")
#mean = 62.84 north, 62.8 south

speed_tues<-subset(M1_data, Day == "Tuesday")
#mean = 62.49 north, 62.23 south

speed_wed<-subset(M1_data, Day == "Wednesday")
#mean = 62.03 north, 61.67 south

speed_thur<-subset(M1_data, Day == "Thursday")
#mean = 62.73 north, 63.12 south

speed_frid<-subset(M1_data, Day == "Friday")
#mean = 61.67 north, 62.65 south

speed_sat<-subset(M1_data, Day == "Saturday")
#mean = 65.66 north, 65.29 south

speed_sun<-subset(M1_data, Day == "Sunday")
#mean = 65.41 north, 65.3 south

day<-c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday")
day_numeric<-c(1,2,3,4,5,6,7)
mean_speed_north<-c(62.84,62.49,62.03,62.73,61.67,65.66,65.41)
mean_speed_south<-c(62.80,62.23,61.67,63.12,62.65,65.29,65.30)
M1_avg_speed<-data.frame(day, mean_speed_north, mean_speed_south)
M1_avg_speed_num<-data.frame(day_numeric, mean_speed_north, mean_speed_south)


plot(mean_speed_north ~ day_numeric, data = M1_avg_speed_num, main = "Mean Speed North vs. Day")
plot(mean_speed_south ~ day_numeric, data = M1_avg_speed_num, main = "Mean Speed South vs. Day")

# Fit a second-degree polynomial regression model NORTHBOUND
poly_mod_north <- lm(mean_speed_north ~ poly(day_numeric, 2), data = M1_avg_speed_num)
poly_mod_north
# Plot the data and the model predictions
plot(M1_avg_speed_num$day_numeric, M1_avg_speed_num$mean_speed_north, xlab = "Day (numeric)",
     ylab = "Mean speed (north)", main = "Mean Speed North vs. Day (Polynomial)")
lines(M1_avg_speed_num$day_numeric, predict(poly_mod_north), col = "red")


pred_df_north <- data.frame(day_numeric = 1:7, 
                      predicted_speed_north = predict(poly_mod_north, newdata = data.frame(day_numeric = 1:7)))
View(pred_df_north)
predicted_speed_north <- pred_df_north$predicted_speed_north

error_n <- M1_avg_speed_num$mean_speed_north - predicted_speed_north
pred_df_north <- data.frame(day_numeric = 1:7,
                      actual_mean_speed = mean_speed_north, predicted_mean_speed = predicted_speed_north,
                      error = error_n)
View(pred_df_north)

# Fit a second-degree polynomial regression model SOUTHHBOUND

poly_mod_south <- lm(mean_speed_south ~ poly(day_numeric, 2), data = M1_avg_speed_num)
poly_mod_south
# Plot the data and the model predictions
plot(M1_avg_speed_num$day_numeric, M1_avg_speed_num$mean_speed_south, xlab = "Day (numeric)", 
     ylab = "Mean speed (south)", main = "Mean Speed South vs. Day (Polynomial)")
lines(M1_avg_speed_num$day_numeric, predict(poly_mod_south), col = "red")


pred_df_south <- data.frame(day_numeric = 1:7, 
                            predicted_speed_south = predict(poly_mod_south, newdata = data.frame(day_numeric = 1:7)))
View(pred_df_south)
predicted_speed_south <- pred_df_south$predicted_speed_south

error_s <- M1_avg_speed_num$mean_speed_south - predicted_speed_south
pred_df_south <- data.frame(day_numeric = 1:7,
                            actual_mean_speed = mean_speed_south, predicted_mean_speed = predicted_speed_south,
                            error = error_s)
View(pred_df_south)

cor(rank(M1_avg_speed_num$day_numeric), rank(M1_avg_speed_num$mean_speed_north), method = "spearman")
cor(rank(M1_avg_speed_num$day_numeric), rank(M1_avg_speed_num$mean_speed_south), method = "spearman")

#Replacing the days of the week with numerical values from 1 to 7 to use plot
M1_avg_speed_numerical<-M1_avg_speed
M1_avg_speed_numerical[M1_avg_speed_numerical=="Monday"]<-1
M1_avg_speed_numerical[M1_avg_speed_numerical=="Tuesday"]<-2
M1_avg_speed_numerical[M1_avg_speed_numerical=="Wednesday"]<-3
M1_avg_speed_numerical[M1_avg_speed_numerical=="Thursday"]<-4
M1_avg_speed_numerical[M1_avg_speed_numerical=="Friday"]<-5
M1_avg_speed_numerical[M1_avg_speed_numerical=="Saturday"]<-6
M1_avg_speed_numerical[M1_avg_speed_numerical=="Sunday"]<-7

#plotting the day vs mean speed north and mean speed south 
plot(M1_avg_speed_numerical$day, 
     M1_avg_speed_numerical$mean_speed_north, 
     type = "b", col = "red", xlab = "Day of the week", ylab = "Mean speed north",
     main = "Mean Speeds (North and South Throughout the Week")
lines(M1_avg_speed_numerical$day, M1_avg_speed_numerical$mean_speed_south, col = "blue", type='b')
legend("topleft", legend = c("Northbound", "Southbound"), col = c("red", "blue"), lty = 1)

#Creating a bar chart of the average north speed and south speed for each day
barplot(t(as.matrix(M1_avg_speed[,2:3])), 
        beside = TRUE, col = c("red", "blue"), names.arg=M1_avg_speed[,1], main = "North vs. South Speeds")
legend("bottomright", legend = c("Northbound", "Southbound"), col = c("red", "blue"), lty = 1)

#Running the t-test, to see if we reject or fail to reject the null hypothesis that
#there is no significant difference between the mean of the two samples

t.test(M1_avg_speed$mean_speed_north, M1_avg_speed$mean_speed_south, paired = TRUE)

#The p-value of 0.8656 is greater than the typical significance level of 0.05, 
#which means there is insufficient evidence to reject the null hypothesis. 
#Therefore, we can conclude that there is not a statistically significant 
#difference between the mean speed of the northbound traffic and the mean 
#speed of the southbound traffic.


road_conditions_df<-M1_speed_data[,c('Speed_N', 'Information_N', 'Speed_S', 'Information_S')]
colnames(road_conditions_df)[2]  <- "Road_conditions_N" 
colnames(road_conditions_df)[4]  <- "Road_conditions_S"

#Changing every value of in Road_Condition_N column that is not NA to 1
road_conditions_df$Road_conditions_N[!is.na(road_conditions_df$Road_conditions_N)] <- "Road Warnings"
#Changing every value of in Road_Condition_N column that is NA to 0
road_conditions_df$Road_conditions_N[is.na(road_conditions_df$Road_conditions_N)] <- "Clear"

#Changing every value of in Road_Condition_S column that is not NA to 1
road_conditions_df$Road_conditions_S[!is.na(road_conditions_df$Road_conditions_S)] <- "Road Warnings"
#Changing every value of in Road_Condition_S column that is NA to 0
road_conditions_df$Road_conditions_S[is.na(road_conditions_df$Road_conditions_S)] <- "Clear"
#Plotting the Speeds when compared to road conditions, where 1 = road work/accidents/congestion
#and where 0 = absence of any traffic information on road conditions 
ggplot(road_conditions_df, aes(Road_conditions_N, Speed_N))+geom_boxplot()
ggplot(road_conditions_df, aes(Road_conditions_S, Speed_S))+geom_boxplot()


#t-test on both north and southbound speeds, testing the hypothesis that
#the mean of the two group of road conditions (1 and 0) have the same mean


t.test(Speed_N~Road_conditions_N, data = road_conditions_df)
t.test(Speed_S~Road_conditions_S, data = road_conditions_df)

#Percentage of times a Warning message was displayed on the M1 based on past data

warning_north<-sum(road_conditions_df$Road_conditions_N == "Road Warnings")
prob_warning_north<-(warning_north/nrow(road_conditions_df))*100
print(paste("The percentage of times a northbound warning signal was indicated:", 
            round(prob_warning_north, digits = 2)))

warning_south<-sum(road_conditions_df$Road_conditions_S == "Road Warnings")
prob_warning_south<-(warning_south/nrow(road_conditions_df))*100
print(paste("The percentage of times a southbound warning signal was indicated:", 
            round(prob_warning_south, digits = 2)))

#Creating a matrix showing the percentages of times the Northbound road was either Clear or had
#any one of 6 warning messages
M1_data_conditions<-M1_data
View(M1_data_conditions)
M1_data_conditions$Road_condition_north[is.na(M1_data_conditions$Road_condition_north)] <- "Clear"
M1_data_conditions$Road_condition_south[is.na(M1_data_conditions$Road_condition_south)] <- "Clear"

count_n<-table(M1_data_conditions$Road_condition_north)
count_matrix_n<-as.matrix(count_n)

count_percentages_n <- cbind(count_matrix_n, Percentages = round(count_matrix_n/sum(count_matrix_n)*100, 1))
colnames(count_percentages_n) <- c("Counts", "Percentages")
count_percentages_n

#Creating a matrix showing the percentages of times the Southbound road was either Clear or had
#any one of 8 warning messages
count_s<-table(M1_data_conditions$Road_condition_south)
count_matrix_s<-as.matrix(count_s)

count_percentages_s <- cbind(count_matrix_s, Percentages = round(count_matrix_s/sum(count_matrix_s)*100, 1))
colnames(count_percentages_s) <- c("Counts", "Percentages")
count_percentages_s

#Average speed for each junction

junction_df<-M1_speed_data[,c("Junction_N", "Speed_N", "Junction_S", "Speed_S")]

junction_codes<-c("J47-J48", "J46-J47", "J45-J46", "J43|44-J45", "J42-J43|44", "J41-J42",
                  "J40-J41", "J39-J40", "J38-J39", "J37-J38", "J36-J37", "J35A-J36", 
                  "J35-J35A", "J34-J35", "J33-J34", "J32-J33", "J31-J32", "J30-J31", 
                  "J29A-J30", "J29-J29A", "J28-J29", "J27-J28", "J26-J27", "J25-J26", 
                  "J24A-J25", "J24-J24A", "J23A-J24", "J23-J23A", "J22-J23", "J21A-J22", 
                  "J21-J21A", "J20-J21", "J19-J20", "J18-J19", "J17-J18", "J16-J17", 
                  "J15A-J16", "J15-J15A", "J14-J15", "J13-J14", "J12-J13", "J11A-J12", 
                  "J11-J11A", "J10-J11", "J9-J10", "J8-J9", "J7-J8", "J6A-J7", "J6-J6A", 
                  "J5-J6", "J4-J5", "J2-J4", "J1-J2")

# create an empty data frame to store the mean speeds
mean_speeds_df <- data.frame(junction_codes = character(),
                             mean_speed_north = double(),
                             mean_speed_south = double())

# loop through each junction and calculate the mean speeds for both north and south
for (junction in junction_codes) {
  avg_speed <- subset(junction_df, Junction_N == junction)
  mean_speed_north <- mean(avg_speed$Speed_N)
  mean_speed_south <- mean(avg_speed$Speed_S)
  
# add the mean speeds to the empty data frame previously created
  mean_speeds_df <- rbind(mean_speeds_df, data.frame(junction_codes = junction,
                                                     mean_speed_north = mean_speed_north,
                                                     mean_speed_south = mean_speed_south))
}
#bar plot comparing Average junction speed for both northbound and southbound direction
barplot(t(as.matrix(mean_speeds_df[,2:3])), 
        beside = TRUE, col = c("red", "blue"), names.arg=mean_speeds_df[,1], 
        ylim = c(0, 70))
legend("bottomleft",cex=0.75,legend = c("Northbound", "Southbound"), 
       fill =c("red", "blue"))

#Calculating minimum and maximum average speed values for both northbound and 
#southbound directions, and associating the values to their specific junctions
min_row_north <- which.min(mean_speeds_df$mean_speed_north)
slowest_junct_north<-mean_speeds_df$junction_codes[min_row_north]
print(paste("The Slowest Junction Northbound is:", slowest_junct_north))

max_row_north <- which.max(mean_speeds_df$mean_speed_north)
fastest_junct_north<-mean_speeds_df$junction_codes[max_row_north]
print(paste("The Fastest Junction Northbound is:", fastest_junct_north))


min_row_south <- which.min(mean_speeds_df$mean_speed_south)
slowest_junct_south<-mean_speeds_df$junction_codes[min_row_south]
print(paste("The Slowest Junction Southbound is:", slowest_junct_south))

max_row_south <- which.max(mean_speeds_df$mean_speed_south)
fastest_junct_south<-mean_speeds_df$junction_codes[max_row_south]
print(paste("The Fastest Junction Southbound is:", fastest_junct_south))

#Creating histograms with the value of the junction average speed. 
#As seen from the plot, these too have a negative skewness
hist(mean_speeds_df$mean_speed_north, xlab = 'Speed (mph)', 
     main = "Junction Average Northbound Speed")

hist(mean_speeds_df$mean_speed_south, xlab = 'Speed (mph)', 
     main = "Junction Average Southbound Speed")

#Create a density plot of the mean_speed_north and mean_speed_south variable 
#Both these plots show that most of the speeds fall between 60 and 70
#with some outlines below that range. This confirms the negative skewness
ggplot(mean_speeds_df, aes(x = mean_speed_north)) +
  geom_density() +
  labs(x = "Speed (mph)", y = "Density", title = "Desinity Plot of Each Junction Mean Speed (North)")

ggplot(mean_speeds_df, aes(x = mean_speed_south)) +
  geom_density() +
  labs(x = "Speed (mph)", y = "Density,", title = "Desinity Plot of Each Junction Mean Speed (South)")

#Creating a data frame with the average NORTHBOUND speed of each date (day) starting from the first day of data collection
#and ending with the last (22 days)
#Since this is more or less normally distributed data, we can discuss the 68-95-99.7 rule
avg_speed_n <- aggregate(M1_data$Speed_N, by = list(M1_data$Date, M1_data$Day), FUN = mean)
View(avg_speed_n)
colnames(avg_speed_n) <- c("Date", "Day", "AvgSpeed")

plot(avg_speed_n$Date, avg_speed_n$AvgSpeed, main="Scatterplot Mean Speed vs. Date (North)",
     xlab="Date", ylab="Mean Speed (mph)", pch=19)


ggplot(avg_speed_n, aes(x = Date, y = AvgSpeed)) +
  geom_line() + scale_x_date(date_breaks = "2 day")+
  labs(x = "Date", y = "Mean Speed (mph)", title = "Speed over Time (North)")

#Using ggplot for the histogram, as it lets me choose the number of bins unlike the hist() function
ggplot(avg_speed_n, aes(x = AvgSpeed)) +
  geom_histogram(binwidth = 1.5, fill = "steelblue", color = "white") +
  labs(x = "Mean Speed (mph)", y = "Frequency", title = "Histogram of Mean Speed (North)")
#Plotting the density distribution
ggplot(avg_speed_n, aes(x = AvgSpeed)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(x = "Mean Speed (mph)", y = "Density", title = "Density Plot of Mean Speed (North)")

#Plotting the Average speed quantile to check for normality
ggqqplot(avg_speed_n$AvgSpeed, main = "Normal QQ Plot Northbound Mean Speed Over 22 Days")

summary(avg_speed_n)
#mean: 63.47
sd_avg_speed_n<-sd(avg_speed_n$AvgSpeed)
#SD: 2.004
# Conduct a one-sample t-test with hypothesized population mean of 65 mph
t.test(avg_speed_n$AvgSpeed, mu = 65, alternative = 'less')
#H0: The true population mean is assumed to be 65mph Northbound for the overall mean 
#Northbound speed
#p-value = 0.0008677
#Conclusion: The p-value supports the alternative hypothesis that the true population mean 
#Northbound is less than 65mph

#Creating a data frame with the average SOUTHBOUND speed of each date (day) starting from the first day of data collection
#and ending with the last (22 days)
#Since this is more or less normally distributed data, we can discuss the 68-95-99.7 rule
avg_speed_s <- aggregate(M1_data$Speed_S, by = list(M1_data$Date, M1_data$Day), FUN = mean)
View(avg_speed_s)
colnames(avg_speed_s) <- c("Date", "Day", "AvgSpeed")

plot(avg_speed_s$Date, avg_speed_s$AvgSpeed, main="Scatterplot Mean Speed vs. Date (South)",
     xlab="Date ", ylab="mean Speed (mph) ", pch=19)


ggplot(avg_speed_s, aes(x = Date, y = AvgSpeed)) +
  geom_line() + scale_x_date(date_breaks = "2 day")+
  labs(x = "Date", y = "Mean Speed (mph)", title = "Speed over Time (South)")

#Using ggplot for the histogram, as it lets me choose the number of bins unlike the hist() function
ggplot(avg_speed_s, aes(x = AvgSpeed)) +
  geom_histogram(binwidth = 1.5, fill = "steelblue", color = "white") +
  labs(x = "Mean Speed (mph)", y = "Frequency", title = "Histogram of Mean Speed (South)")
#Plotting the density distribution
ggplot(avg_speed_s, aes(x = AvgSpeed)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(x = "mean Speed (mph)", y = "Density", title = "Density Plot of Mean Speed (South)")

#Plotting the Average speed quantile to check for normality
ggqqplot(avg_speed_s$AvgSpeed, main = "Normal QQ Plot Southbound Mean Speed over 22 days")

summary(avg_speed_s)
#mean: 63.48
sd_avg_speed_s<-sd(avg_speed_s$AvgSpeed)
#SD: 1.858
# Conduct a one-sample t-test with hypothesized population mean of 65 mph
t.test(avg_speed_s$AvgSpeed, mu = 65, alternative = 'less')
#H0: The true population mean is assumed to be 65mph Southbound for the overall mean 
#Northbound speed
#p-value = 0.000479
#Conclusion: The p-value supports the alternative hypothesis that the true population mean 
#Southbound is less than 65mph


#Since the sampling distribution of means is almost normally distributed, as seen
#from the previous histograms of 
#Calculating the standard error
stand_error_north<-sd_avg_speed_n/sqrt(nrow(avg_speed_n))
stand_error_north
#Standard error: 0.42
#We can be reasonably sure with 95% certainty that the true population mean
#for the population daily speeds lies between 63.47 +/- 2SE, or 63.47+/-0.84 mph

stand_error_south<-sd_avg_speed_s/sqrt(nrow(avg_speed_s))
stand_error_south
#Standard error: 0.4
#We can be reasonably sure with 95% certainty that the true population mean
#for the population daily speeds lies between 63.48 +/- 2SE, or 63.47+/-0.8 mph


#LOGISTIC REGRESSION NORTHBOUND AND SOUTHBOUND ON PROBABILITY OF OBSERVING A WARNING 
#TRAFFIC SIGN AS SPEED INCREASES

road_conditions_df<-M1_speed_data[,c('Speed_N', 'Information_N', 'Speed_S', 'Information_S')]
View(road_conditions_df)
colnames(road_conditions_df)[2]  <- "Road_conditions_N" 
colnames(road_conditions_df)[4]  <- "Road_conditions_S"
road_conditions_df$Road_conditions_N[!is.na(road_conditions_df$Road_conditions_N)] <- 1

#Changing every value of in Road_Condition_N column that is NA to 0
road_conditions_df$Road_conditions_N[is.na(road_conditions_df$Road_conditions_N)] <- 0
road_conditions_df$Road_conditions_N<-as.numeric(road_conditions_df$Road_conditions_N)

#Changing every value of in Road_Condition_S column that is not NA to 1
road_conditions_df$Road_conditions_S[!is.na(road_conditions_df$Road_conditions_S)] <- 1

#Changing every value of in Road_Condition_S column that is NA to 0
road_conditions_df$Road_conditions_S[is.na(road_conditions_df$Road_conditions_S)] <- 0
road_conditions_df$Road_conditions_S<-as.numeric(road_conditions_df$Road_conditions_S)

#Using logistic regression to estimate the effect of Speed NORTHBOUND on the probability of observing a warning sign
# determined by 1 (warning sign) or 0(No warning sign) 

logist_model_N<-glm(Road_conditions_N~Speed_N, data = road_conditions_df, family = 'binomial')

# Create a sequence of speeds
speeds_northbound <- seq(from = min(road_conditions_df$Speed_N), to = max(road_conditions_df$Speed_N), by = 1)

# Predict the probability of a warning sign at each speed value
probabilities_northbound <- predict(logist_model_N, newdata = data.frame(Speed_N = speeds_northbound), type = "response")

# Create a data frame with the predicted probabilities
predictions_northbound <- data.frame(Speed_N = speeds_northbound, Probabilities = probabilities_northbound)

# Plot the data and the predicted probabilities
ggplot(road_conditions_df, aes(x = Speed_N, y = Road_conditions_N)) +
  geom_point() +
  geom_line(data = predictions_northbound, aes(y = Probabilities), color = "red")+
  ggtitle("Predicted Probabilities of Road Conditions (North)")

summary(logist_model_N)$coef
summary(logist_model_N)

#LOGISTIC RESULT INTEPRETATION NORTHBOUND:
#Deviance residuals: 
#Minimum residual is -2.1565 and the maximum is 2.0281
#The model may not fit well in some cases
#Coefficients:
#For every one unit increase in Speed_N, the expected log-odds of 
#observing a warning sign decrease by 6.7%


#Using logistic regression to estimate the effect of Speed SOUTHBOUND on the probability of observing a warning sign
# determined by 1 (warning sign) or 0(No warning sign) 

logist_model_S<-glm(Road_conditions_S~Speed_S, data = road_conditions_df, family = 'binomial')

# Create a sequence of speeds
speeds_southbound <- seq(from = min(road_conditions_df$Speed_S), to = max(road_conditions_df$Speed_S), by = 1)

# Predict the probability of a warning sign at each speed value
probabilities_southbound <- predict(logist_model_S, newdata = data.frame(Speed_S = speeds_southbound), type = "response")

# Create a data frame with the predicted probabilities
predictions_southbound <- data.frame(Speed_S = speeds_southbound, Probabilities = probabilities_southbound)

# Plot the data and the predicted probabilities
ggplot(road_conditions_df, aes(x = Speed_S, y = Road_conditions_S)) +
  geom_point() +
  geom_line(data = predictions_southbound, aes(y = Probabilities), color = "red")+
  ggtitle("Predicted Probabilities of Road Conditions (South)")

summary(logist_model_S)$coef
summary(logist_model_S)


#LOGISTIC RESULT INTEPRETATION SOUTHBOUND:
#Deviance residuals: 
#Minimum residual is -1.8648 and the maximum is 2.4046
#The model may not fit well in some cases
#Coefficients:
#For every one unit increase in Speed_N, the expected log-odds of 
#observing a warning sign decrease by 6.9%

#CLUSTERING
#NORTH
# Select relevant columns for clustering
set.seed(2023.1)
cluster_data_N <- road_conditions_df[, c("Speed_N", "Road_conditions_N")]
#Turning road condition column into class Factor, since it has binary values that should nto be numerical
cluster_data_N<- data.frame(Speed = road_conditions_df$Speed_N, RoadCondition = factor(road_conditions_df$Road_conditions_N))
# Scale the Speed data
cluster_data_N$Speed <- scale(cluster_data_N$Speed)
cluster_data_N<-sample_n(cluster_data_N, 500, replace = FALSE) #Package dplyr
# Determine the optimal number of clusters
elbow_plot_N <- fviz_nbclust(cluster_data_N, kmeans, method = "wss") #package factoextra
elbow_plot_N
# Perform clustering with the optimal number of clusters
kmeans_model_N <- kmeans(cluster_data_N, 2, nstart = 25)
kmeans_model_N
# Plotting the clusters

ggplot(cluster_data_N, aes(x = Speed, y = RoadCondition, color = as.factor(kmeans_model_N$cluster))) +
  geom_point() +
  labs(title = "Clustering of Speed and Road Conditions on the M1 Northbound",
       x = "Speed",
       y = "Road Conditions")+scale_color_discrete(name = "Cluster Group"
                                                     , labels = c("Cluster 1", "Cluster 2"))

#SOUTH
# Select relevant columns for clustering
set.seed(2023.2)
cluster_data_S <- road_conditions_df[, c("Speed_S", "Road_conditions_S")]
#Turning road condition column into class Factor, since it has binary values that should nto be numerical
cluster_data_S<- data.frame(Speed = road_conditions_df$Speed_S, RoadCondition = factor(road_conditions_df$Road_conditions_S))
# Scale the Speed data
cluster_data_S$Speed <- scale(cluster_data_S$Speed)
cluster_data_S<-sample_n(cluster_data_S, 500, replace = FALSE) #Package dplyr
# Determine the optimal number of clusters
elbow_plot_S <- fviz_nbclust(cluster_data_S, kmeans, method = "wss") #package factoextra
elbow_plot_S
# Perform clustering with the optimal number of clusters
kmeans_model_S <- kmeans(cluster_data_S, 2, nstart = 25)
kmeans_model_S
# Plotting the clusters

ggplot(cluster_data_S, aes(x = Speed, y = RoadCondition, color = as.factor(kmeans_model_S$cluster))) +
  geom_point() +
  labs(title = "Clustering of Speed and Road Conditions on the M1 Southbound",
       x = "Speed",
       y = "Road Conditions")+scale_color_discrete(name = "Cluster Group"
                                                     , labels = c("Cluster 1", "Cluster 2"))
