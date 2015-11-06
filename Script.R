library(dplyr)
library(ggplot2)

#Load data
raw.data <- read.csv("household_power_consumption.txt", sep=";", stringsAsFactors = FALSE)
#Transform data and filter
raw.data <- mutate(raw.data, DateTime = paste(Date, Time, sep=" "), Date = as.Date(Date,"%d/%m/%Y"))
raw.data <- mutate(raw.data, DateTime =as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
data <- filter(raw.data, Date >= "2007-02-01" & Date <= "2007-02-02")
#Check for missings, coded as "?"
dataCheck <- data[mapply(function(x) x=="?", data[,-c(1:2,10)])]
print(dataCheck)
#Transform data to numeric, as no missings in required period
data <- mutate(data,Global_active_power=as.numeric(Global_active_power),Global_reactive_power=as.numeric(Global_reactive_power),Voltage=as.numeric(Voltage), 
               Global_intensity=as.numeric(Global_intensity), Sub_metering_1=as.numeric(Sub_metering_1),Sub_metering_2=as.numeric(Sub_metering_2), 
               Sub_metering_3= as.numeric(Sub_metering_3))


#Plot 1
plot1 <- ggplot(data=data, aes(x=Global_active_power)) + geom_histogram(binwidth=0.5, fill="red", col="black") + theme_bw() + theme(panel.grid.major = element_blank(), 
                                                                                                                           panel.grid.minor=element_blank()) + 
  xlab("Global Active Power (kilowatts)") + ylab("Frequency") + ggtitle("Global Active Power")

ggsave('plot1.png', plot1)

library(scales)
plot2 <- ggplot(data=data, aes(x=DateTime, y=Global_active_power)) + geom_line() + scale_x_datetime(breaks = date_breaks("1 day"),labels=date_format("%a")) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())  + ylab("Global Active Power (kilowatts)") + xlab(" ")
ggsave('plot2.png', plot2)

library(reshape2)
#Reshape data so that group plot is possible with ggplot2. As posixct not supported in reshape2, transformation to character and back required
newDataSet <- transmute(data, DateTime=as.character(DateTime), Sub_metering_1= Sub_metering_1,Sub_metering_2=Sub_metering_2,Sub_metering_3=Sub_metering_3)
dataMelted <-melt(newDataSet, variable.names=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), id.vars="DateTime")
dataMelted <- mutate(dataMelted, DateTime =as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))


plot3 <- ggplot(data=dataMelted, aes(x=DateTime,y=value, group=variable, color=variable)) + geom_line() + scale_x_datetime(breaks = date_breaks("1 day"),labels=date_format("%a")) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank()) + xlab("") + ylab("Energy Sub Meetering") + 
  theme(legend.position = c(0, 1), 
       legend.justification = c(0, 1)) 
ggsave('plot3.png')

library(gridExtra)

plot4 <- ggplot(data=data,aes(x=DateTime, y=Voltage)) + geom_line() + theme_bw() + xlab("datetime") + scale_x_datetime(breaks = date_breaks("1 day"),labels=date_format("%a")) +
 theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())
plot5 <- ggplot(data=data,aes(x=DateTime, y=Global_reactive_power)) + geom_line() + theme_bw() + xlab("datetime") + scale_x_datetime(breaks = date_breaks("1 day"),labels=date_format("%a"))  + 
    theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())

png("plot4.png")
grid.arrange(plot2, plot4, plot3, plot5, ncol=2)
dev.off()

ggsave('plot4.png', plot=plotCollected)
