## This function recreates plot 2, as per the Project 1 description
plot2 <- function() {
    ##Read household power data, first two cols read as type Date, the rest read as char with NA <--> ?
    data<-fread("household_power_consumption.txt",sep=";",na.strings=c("NA","?",""),colClasses=c(rep("Date",2),rep("character",7)))
  
    ## Convert date to and time single date format and active power to numeric
    date.toplot<-as.POSIXct(paste(data$Date,data$Time),format="%d/%m/%Y %H:%M:%S")
    data$Global_active_power<-as.numeric(data$Global_active_power)
  
    ## Set the start and end times for data analysis and extract the Global active power for those dates
    start1<-as.POSIXct("01/02/2007 00:00:00",format="%d/%m/%Y %H:%M:%S")
    end1<-as.POSIXct("02/02/2007 23:59:59",format="%d/%m/%Y %H:%M:%S")
    toplot<-data$Global_active_power[(date.toplot>=start1)&(date.toplot<=end1)]
    date.toplot<-date.toplot[(date.toplot>=start1)&(date.toplot<=end1)]
    
    ## remove the na's from the data to be plotted
    date.toplot<-date.toplot[!is.na(toplot)]
    toplot<-toplot[!is.na(toplot)]
  
    ## initialize plot, set x, y axis label accordingly
    plot(date.toplot,toplot,ylab="Global Active Power (kilowatts)",xlab="",type="l")
  
    ## Print to file and close file if necessary
    dev.print(png,file="plot2.png",width=480,height=480)
    if (names(dev.cur())=="png") dev.off()
}