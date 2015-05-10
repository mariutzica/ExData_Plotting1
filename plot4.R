plot4 <- function() {
  ##Read household power data, first two cols read as type Date, the rest read as char with NA <--> ?
  data<-fread("household_power_consumption.txt",sep=";",na.strings=c("NA","?",""),colClasses=c(rep("Date",2),rep("character",7)))
  
  ## Convert date to and time single date format and active power to numeric
  date.toplot<-as.POSIXct(paste(data$Date,data$Time),format="%d/%m/%Y %H:%M:%S")
  data$Sub_metering_1=as.numeric(data$Sub_metering_1)
  data$Sub_metering_2=as.numeric(data$Sub_metering_2)
  data$Sub_metering_3=as.numeric(data$Sub_metering_3)
  data$Global_active_power<-as.numeric(data$Global_active_power)
  data$Voltage<-as.numeric(data$Voltage)
  data$Global_reactive_power<-as.numeric(data$Global_reactive_power)
  
  ## Set the start and end times for data analysis and extract the Global active power for those dates
  start1<-as.POSIXct("01/02/2007 00:00:00",format="%d/%m/%Y %H:%M:%S")
  end1<-as.POSIXct("02/02/2007 23:59:59",format="%d/%m/%Y %H:%M:%S")
  range<-(date.toplot>=start1)&(date.toplot<=end1)
  toplot<-data$Global_active_power[(date.toplot>=start1)&(date.toplot<=end1)]
  toplot1<-data$Sub_metering_1[(date.toplot>=start1)&(date.toplot<=end1)]
  toplot2<-data$Sub_metering_2[(date.toplot>=start1)&(date.toplot<=end1)]
  toplot3<-data$Sub_metering_3[(date.toplot>=start1)&(date.toplot<=end1)]
  ##voltage<-data$Voltage[(date.toplot>=start1)&(date.toplot<=end1)]
  ##reactive_power<-data$Global_reactive_power[(date.toplot>=start1)&(date.toplot<=end1)]
  date.toplot<-date.toplot[(date.toplot>=start1)&(date.toplot<=end1)]
  
  ## remove the na's from the data to be plotted
  date.toplot1<-date.toplot[!is.na(toplot1)&!is.na(toplot2)&!is.na(toplot3)]
  date.toplot0<-date.toplot[!is.na(toplot)]
  toplot<-toplot[!is.na(toplot)]
  toplot1<-toplot1[!is.na(toplot1)]
  toplot2<-toplot2[!is.na(toplot2)]
  toplot3<-toplot3[!is.na(toplot3)]
  
  ## initialize plot, set x, y axis label accordingly
  png("plot4.png",width=480,height=480)
  
  ##set up grid of plots to fill in 1 column at a time
  par(mfcol=c(2,2))
  
  ##first plot, Global active power, line
  plot(date.toplot0,toplot,ylab="Global Active Power (kilowatts)",xlab="",type="l")
  
  ##second plot, sub metering data 1,2,3
  ##new legend has no border
  plot(date.toplot1,toplot1,type="l",xlab="",ylab="Energy sub metering")
  lines(date.toplot1,toplot2,col="red")
  lines(date.toplot1,toplot3,col="blue")
  legend("topright",legend=names(data)[seq(7,9)],lty="solid",col=c("black","red","blue"),border="none")
  
  ##third plot voltage, line plot
  plot(date.toplot,data$Voltage[range],xlab="datetime",ylab="Voltage",type="l")
  
  ##fourth plot reactive power, line plot
  plot(date.toplot,data$Global_reactive_power[range],xlab="datetime",type="l",ylab=names(data)[4])
  dev.off()
}