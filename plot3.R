## This function recreates plot 2, as per the Project 1 description
plot3 <- function() {
    ##Read household power data, first two cols read as type Date, the rest read as char with NA <--> ?
    data<-fread("household_power_consumption.txt",sep=";",na.strings=c("NA","?",""),colClasses=c(rep("Date",2),rep("character",7)))
  
    ## Convert date to and time single date format and active power to numeric
    date.toplot<-as.POSIXct(paste(data$Date,data$Time),format="%d/%m/%Y %H:%M:%S")
    data$Sub_metering_1=as.numeric(data$Sub_metering_1)
    data$Sub_metering_2=as.numeric(data$Sub_metering_2)
    data$Sub_metering_3=as.numeric(data$Sub_metering_3)
  
    ## Set the start and end times for data analysis and extract the Global active power for those dates
    start1<-as.POSIXct("01/02/2007 00:00:00",format="%d/%m/%Y %H:%M:%S")
    end1<-as.POSIXct("02/02/2007 23:59:59",format="%d/%m/%Y %H:%M:%S")
    toplot1<-data$Sub_metering_1[(date.toplot>=start1)&(date.toplot<=end1)]
    toplot2<-data$Sub_metering_2[(date.toplot>=start1)&(date.toplot<=end1)]
    toplot3<-data$Sub_metering_3[(date.toplot>=start1)&(date.toplot<=end1)]
    date.toplot<-date.toplot[(date.toplot>=start1)&(date.toplot<=end1)]
  
    ## remove the na's from the data to be plotted
    date.toplot<-date.toplot[!is.na(toplot1)&!is.na(toplot2)&!is.na(toplot3)]
    toplot1<-toplot1[!is.na(toplot1)]
    toplot2<-toplot2[!is.na(toplot2)]
    toplot3<-toplot3[!is.na(toplot3)]
  
    ## initialize plot, set x, y axis label accordingly
    png("plot3.png",width=480,height=480)
    plot(date.toplot,toplot1,type="l",xlab="",ylab="Energy sub metering")
    lines(date.toplot,toplot2,col="red")
    lines(date.toplot,toplot3,col="blue")
    legend("topright",legend=names(data)[seq(7,9)],lty="solid",col=c("black","red","blue"))
    dev.off()
}