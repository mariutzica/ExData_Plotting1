## This function recreates plot 1, as per the Project 1 description
plot1 <- function() {
    ##Read household power data, first two cols read as type Date, the rest read as char with NA <--> ?
    data<-fread("household_power_consumption.txt",sep=";",na.strings=c("NA","?",""),colClasses=c(rep("Date",2),rep("character",7)))
    
    ## Convert date to Date format and active power to numeric
    data$Date<-as.Date(data$Date,format="%d/%m/%Y")
    data$Global_active_power<-as.numeric(data$Global_active_power)

    ## Set the start and end times for data analysis and extract the Global active power for those dates
    start1<-as.Date("01/02/2007",format="%d/%m/%Y")
    end1<-as.Date("02/02/2007",format="%d/%m/%Y")
    toplot<-subset(data,(Date>=start1)&(data$Date<=end1),select=Global_active_power]

    ## remove the na's from the data to be plotted
    toplot<-toplot[!is.na(toplot)]

    ## initialize histogram, set bar color, title, and x axis label accordingly
    hist(toplot,col='red',main="Global Active Power",xlab="Global Active Power (kilowatts)")

    ## Print to file and close file if necessary
    dev.print(png,file="plot1.png",width=480,height=480)
    if (names(dev.cur())=="png") dev.off()
}