library(plot3D)
library(reshape)
library(data.table)
library(rcpp)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(scales)
library(dplyr)
Sys.setenv(JAVA_HOME='C:\\PROGRA~1\\Java\\JDK18~1.0_1')
library(rJava)
library(xlsxjars)
library(xlsx)
library(gtable)
library(grid)
library(RColorBrewer)
library(sqldf)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
#library(xlsxboost)
library(DBI)
library(rgdal)
library(sf) 
library(usmap)



library(lubridate)


#library(openxlsx)
setwd("S:/Water model standalone/Water_Avail/exercise")

ERCOT_DA_Prices_2016 <- read.csv(file = 'energy-analyst-data-exercise-public-master-historicalPriceData/energy-analyst-data-exercise-public-master-historicalPriceData/historicalPriceData/ERCOT_DA_Prices_2016.csv')

ERCOT_DA_Prices_2017 <- read.csv(file = 'energy-analyst-data-exercise-public-master-historicalPriceData/energy-analyst-data-exercise-public-master-historicalPriceData/historicalPriceData/ERCOT_DA_Prices_2017.csv')

ERCOT_DA_Prices_2018 <- read.csv(file = 'energy-analyst-data-exercise-public-master-historicalPriceData/energy-analyst-data-exercise-public-master-historicalPriceData/historicalPriceData/ERCOT_DA_Prices_2018.csv')

ERCOT_DA_Prices_2019 <- read.csv(file = 'energy-analyst-data-exercise-public-master-historicalPriceData/energy-analyst-data-exercise-public-master-historicalPriceData/historicalPriceData/ERCOT_DA_Prices_2019.csv')

#Task 1: 
ERCOT_DA_Prices <- rbind(ERCOT_DA_Prices_2016, ERCOT_DA_Prices_2017, ERCOT_DA_Prices_2018, ERCOT_DA_Prices_2019)

#Task 2:
ERCOT_DA_Prices$Date2 <- as.POSIXct(ERCOT_DA_Prices$Date, format="%Y-%m-%d %H:%M:%S")
ERCOT_DA_Prices$Year <- format(ERCOT_DA_Prices[,"Date2"], "%Y")
ERCOT_DA_Prices$Month <- format(ERCOT_DA_Prices[,"Date2"], "%m")
  
avg_price <- aggregate(Price~SettlementPoint+Year+Month, ERCOT_DA_Prices, mean)
colnames(avg_price)[colnames(avg_price) == "Price"] <- "AveragePrice"
#Task 3:
write.csv(avg_price,'Output/AveragePriceByMonth.csv')

#Task 4
ERCOT_DA_Prices2 <- subset(ERCOT_DA_Prices, grepl("HB_", ERCOT_DA_Prices$SettlementPoint)&ERCOT_DA_Prices$Price >0 )
ERCOT_DA_Prices2$log_price <- log(ERCOT_DA_Prices2$Price)
hourly_volatility <- aggregate(ERCOT_DA_Prices2$log_price~SettlementPoint+Year, ERCOT_DA_Prices2, sd)
colnames(hourly_volatility)[colnames(hourly_volatility) == "ERCOT_DA_Prices2$log_price"] <- "HourlyVolatility"
#Task 5
write.csv(hourly_volatility,'Output/HourlyVolatilityByYear.csv')
#Task 6
#Max_hourly_volatility <- aggregate(HourlyVolatility~Year, hourly_volatility, max)
#library(data.table)
#hourly_volatility[ , .SD[which.max(HourlyVolatility)], by =Year]

#hourly_volatility[hourly_volatility[ , .I[which.min(HourlyVolatility)], by = Year]$V1]
   
Max_hourly_volatility <- as.data.frame(hourly_volatility %>% 
  group_by(Year) %>% 
  slice(which.max(HourlyVolatility)))
write.csv(hourly_volatility,'Output/MaxVolatilityByYear.csv')
#Task 7
#assumption: translated 0:23hour format to 1:24hour format
for (i in unique(ERCOT_DA_Prices$SettlementPoint)){
  
    subs1 <- ERCOT_DA_Prices[ERCOT_DA_Prices$SettlementPoint == i,]
    subs1$DayMonthYear <- format(subs1[,"Date2"], "%d/%m/%Y")
    subs1$Hour <- format(subs1[,"Date2"], "%H")
    
    #subs1$Hour <- as.numeric(as.character(subs1$Hour))
    subs1 <- subs1[ , which(names(subs1) %in% c("SettlementPoint","Price","DayMonthYear","Hour"))]
    subs2 <- reshape(subs1, idvar = c("SettlementPoint","DayMonthYear"), timevar = "Hour", direction = "wide")
    colnames(subs2)[3:26] <- 1:24
    colnames(subs2)[3:26] <- sub("^", "X", colnames(subs2)[3:26] ) 
    colnames(subs2)[colnames(subs2) == "SettlementPoint"] <- "Variable"
    colnames(subs2)[colnames(subs2) == "DayMonthYear"] <- "Date"
    subs2 <- subs2 %>% mutate(Date = format(as.Date(Date, "%d/%m/%Y")))
    #colnames(subs2) <- gsub('%d', 'X%d', colnames(subs2), fixed=TRUE)
    #subs2$Date <- format(as.Date(subs2$Date), "%Y/%m/%d")
    
    #assign(paste("sl_occtype_buildingtypt", i, sep = "-"), cate_results22 )
    write.csv(subs2, file = paste0("Output/formattedSpotHistory/spot_", i, ".csv"), row.names=FALSE)
  
}
#Bonus- Mean Plots
hub_settlementpoint <- subset(ERCOT_DA_Prices, grepl("HB_", ERCOT_DA_Prices$SettlementPoint))
load_settlementpoint <- subset(ERCOT_DA_Prices, grepl("LZ_", ERCOT_DA_Prices$SettlementPoint))

hub_settlementpoint$Date2 <- as.Date(as.POSIXct(hub_settlementpoint$Date2))
ggplot(hub_settlementpoint, aes(x = Date2, y = Price, group = SettlementPoint, color=SettlementPoint )) +
  geom_line()+labs(title = "Monthly average price for hubs", x = "Year", y = "Monthly Average Price")+scale_x_date(labels = date_format("%Y-%m-%d"))
#+scale_x_datetime(date_breaks = "1 month", labels = date_format("%Y-%m-%d"))
ggsave(path = paste0(getwd(),"/Output"), filename = "SettlementHubAveragePriceByMonth.png")

ggplot(load_settlementpoint, aes(x = Date2, y = Price, group = SettlementPoint, color=SettlementPoint )) +
  geom_line()+labs(title = "Monthly average price for loads", x = "Year", y = "Monthly Average Price")
ggsave(path = paste0(getwd(),"/Output"), filename = "loadZoneAveragePriceByMonth.png")

#Bonus - Volatility Plots
ggplot(hourly_volatility, aes(x = Year, y = HourlyVolatility, group = SettlementPoint, color=SettlementPoint )) +
  geom_line(size=1)+labs(title = "Hourly volatility across settlement hubs", x = "Year", y = "Hourly volatility")
ggsave(path = paste0(getwd(),"/Output"), filename = "VolatilityPlot.png")
