# Usage:
# 1. Put data next to the script
# 2. Load the function source(<path to script>)
# 3. Execute function MakePlot4(<filename data>)
# 4. Look to plot

MakePlot4 <- function(FileName = "household_power_consumption.txt") {
  
  # Set working directory
  # Arg 'FileName' has a default value 'household_power_consumption.txt' 
  setwd(getSrcDirectory(MakePlot4))
  
  # Try read data 
  if (!file.exists(FileName)) return(paste('There is no such file in the directory',getwd()))
  
  # Excellent! File exists, let's read raw data .. 
  w_src <- read.table(file = FileName, header = TRUE, sep = ";", stringsAsFactors = FALSE)
  # than remove missing values ..
  w_clean <- w_src[complete.cases(w_src),]
  
  # Ok, let's convert data/time ..
  w_clean$Time <- strptime(paste(w_clean$Date,w_clean$Time),format = "%d/%m/%Y %H:%M:%S")
  
  # Select range ..
  rangeFrom <- strptime("01/02/2007 00:00:00",format = "%d/%m/%Y %H:%M:%S")
  rangeTill <- strptime("02/02/2007 23:59:59",format = "%d/%m/%Y %H:%M:%S")
  
  # Make query
  query <- subset(w_clean, w_clean$Time >= rangeFrom & w_clean$Time <= rangeTill)
  
  Sys.setlocale("LC_ALL","English")
  
  query$Global_active_power <- as.numeric(query$Global_active_power)
  query$Global_reactive_power <- as.numeric(query$Global_reactive_power)
  query$Sub_metering_1 <- as.numeric(query$Sub_metering_1)
  query$Sub_metering_2 <- as.numeric(query$Sub_metering_2)
  query$Voltage <- as.numeric(query$Voltage)
 
  #  Set output to new window (comment this line, if OS != Windows)
  windows()
  
  # Make plot #4 and copy to PNG
  layout(matrix(1:4, ncol = 2))
  with(query, plot(query$Time, query$Global_active_power,type="l",ylab = "Global Active Power (kilowatts)", xlab = ""))
  
  plot(query$Time, query$Sub_metering_1,type="l",xlab = "",ylab = "Energy sub metering")
  lines(x= query$Time, query$Sub_metering_2, col="red")
  lines(x= query$Time, query$Sub_metering_3, col="blue")
  legend("topright",legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"),lty=1,cex =0.7,bty = "n",y.intersp = 0.6,x.intersp = 0.5)

  with(query, plot(query$Time, query$Voltage, type="l", xlab="datetime", ylab = "Voltage"))
  with(query, plot(query$Time, query$Global_reactive_power, type="l", xlab="datetime", ylab = "Global_reactive_power"))
  
  dev.copy(png,"plot4.png", width=480, height=480)
  
  dev.off()
}