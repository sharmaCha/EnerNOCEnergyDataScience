#Forecasting
library(gdata)
#forecast <- read.xls
forecast <- read.csv("E:/ADS/Assignment 1/forecastData.csv")
colnames(forecast) <- c("Day", "Hour", "Temp");
forecastInput <- forecast

#Rearranging the column
library(lubridate)
forecast$Date <- as.Date(forecast$Day, "%m/%d/%Y")
forecast <- forecast[order(forecast$Date),]

forecast$month <- month(forecast$Date)
forecast$day <- day(forecast$Date)
forecast$year <- year(forecast$Date)

#finding day of week
forecast["Day of Week"] <- NULL
forecast["Day of Week"] <- factor(weekdays(forecast$Date), labels = c(0,1,2, 3, 4, 5, 6),
                                levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#finding weekday
library("timeDate")
forecast["Weekday"] <- NULL
forecast["Weekday"] <- factor(isWeekday(forecast$Date, wday = 1:5), labels = c("1", "0"),
                              levels = c(TRUE, FALSE))

#Evaluating the peak hour
#lapply(forecast$hour function(x) if(x > 6 && x < 20) forecast$Peakhour <- 1 else forecast$Peakhour <- 0)
for(i in seq(from=1, to=nrow(forecast), by=1)) {
  
  if(forecast$Hour > 6 && forecast$Hour < 20)
    forecast$Peakhour <- 1
  else
    forecastPeakhour <- 0
}

#Removing unneccesary column
forecast <- forecast[ , -which(names(forecast) %in% c("Day"))]

#View(forecast)
write.csv(forecast, file="Output/forecastInput.csv")

#predict the power usage in KWH for each Hour.
library(forecast)
pred = predict(lm.fit, forecast)
#accuracy(pred, test$kWh)
KWH <- pred
#View(pred)

forecastOutput <- cbind(forecastInput,KWH)
View(forecastOutput)

#writing the data to forecastData.csv
filename <- paste("Output/forecastOutput_Account_",unique(energy$Account), ".csv", sep = "")
filename;
write.csv(forecastOutput, file=filename)
