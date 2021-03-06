Get_Plot3 <- function(DataFilePath)
{
  # Reading Data 
  HouseHoldDataTable <- 
    read.table(DataFilePath,
               header=TRUE, sep=";", na.strings = "?", 
               colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
  ##Setting Date Column Class
  HouseHoldDataTable$Date <- as.Date(HouseHoldDataTable$Date, "%d/%m/%Y")
  
  ##Filtering Data to required range
  HouseHoldDataTable <- 
    subset(HouseHoldDataTable,Date >= as.Date("2007-2-1") 
           & Date <= as.Date("2007-2-2"))
  
  ##Cleaning incomplete cases
  HouseHoldDataTable <-
    HouseHoldDataTable[complete.cases(HouseHoldDataTable),]
  
  ## merging date and time columns into one
  dateTime <- paste(HouseHoldDataTable$Date, HouseHoldDataTable$Time)
  
  dateTime <- setNames(dateTime, "DateTime")
  
  HouseHoldDataTable <- HouseHoldDataTable[ ,!(names(HouseHoldDataTable) %in% c("Date","Time"))]
  
  HouseHoldDataTable <- cbind(dateTime, HouseHoldDataTable)
  
  HouseHoldDataTable$dateTime <- as.POSIXct(dateTime)
  
  # Generating the Plot
  with(HouseHoldDataTable, {
    plot(Sub_metering_1~dateTime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
  })
  legend("topright",   col=c("black", "red", "blue"), lwd=c(1,1,1), 
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  ## Saving the File to PNG
  dev.copy(png,"plot3.png", width=480, height=480)
  dev.off()
}