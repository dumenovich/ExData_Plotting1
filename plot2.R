# Usage:
# 1. Put data next to the script
# 2. Load the function source(<path to script>)
# 3. Execute function MakePlot2(<filename data>)
# 4. Look to plot

MakePlot2 <- function(FileName = "household_power_consumption.txt") {
  
  # Set working directory
  # Arg 'FileName' has a default value 'household_power_consumption.txt' 
  setwd(getSrcDirectory(MakePlot2))
  
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
  
  query$Global_active_power<-as.numeric(query$Global_active_power)

  #  Set output to new window (comment this line, if OS != Windows)
  windows()
  
  # Make plot #2 and copy to PNG
  par(mfrow = c(1,1))
  with(query, plot(Time, Global_active_power, type="l", ylab = "Global Active Power (kilowatts)", xlab = ""))
  
  dev.copy(png,"plot2.png", width=480, height=480)
  
  dev.off()  
}