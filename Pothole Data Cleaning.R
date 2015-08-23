setwd("H:/USER/JGenser/Pot Holes/Program")

library(plyr)
library(ggplot2)
library(ggmap)
library(sp)
library(lubridate)
library(leaflet)

bos = read.csv("Boston_Pothole_Cases.txt", header=T, sep="\t")
cam = read.csv("Cambridge_Pot_Holes.csv", header=T, sep=",")

cam2=cam
#convert the Cambridge location information into lattitudes and longitudes
cam$TEMP = gsub(".*\n|).*","",cam$Address)
cam$LATITUDE = as.numeric(substr(cam$TEMP,2,11))
cam$LONGITUDE = as.numeric(substr(gsub(".*,","",cam$TEMP),2,12))

#remove records with missing location information from the Cambridge data subset
cam = subset(cam, is.na(cam$LATITUDE) == F)

#convert daates to an R-usable format
bos$dt_close = as.Date(bos$date_closed, origin = "1899-12-30")
cam$dt_close = as.Date(cam$date_closed, origin = "1899-12-30")
bos$dt_open = as.Date(bos$date_open, origin = "1899-12-30")
cam$dt_open = as.Date(cam$date_open, origin = "1899-12-30")

#create placeholder fields in data in order to stack them
cam$Source <- ""
bos$Status <- ""

keep = c("Status","Source" ,"LATITUDE", "LONGITUDE", "dt_close", "dt_open" )

cam2 = cam[keep]
bos2 = bos[keep]

#lag_counts = count(cam2[, c("LATITUDE", "LONGITUDE")])
#lag_counts = table(bos2[, c("LATITUDE", "LONGITUDE")])

df_combined = rbind(cam2,bos2)
lag_counts = count(df_combined[, c("LATITUDE", "LONGITUDE")])

many_fills = lag_counts[ lag_counts$freq > 10,]

cam2 = cam2[month(cam2$dt_close)<4 & year(cam2$dt_close) ==2015,]
bos2 = bos2[month(bos2$dt_close)<4 & year(bos2$dt_close) ==2015,]
# cam2 = subset(cam2, (year(dt_close) == 2015 & month(dt_close<4)))
# bos2 = subset(bos2, (year(dt_close) == 2015 & month(dt_close<4)))


##combine cambridge and boston datasets
df <- rbind(cam2, bos2)



##calculate the time to closure in days
df$lag = as.numeric(difftime(df$dt_close, df$dt_open, units="days"))
df$lag[which(df$lag < -1 )] <- -1

write.table(df, "mappingData.csv", sep="," , col.names=NA)
write.table(many_fills, "manyFills.csv", sep=",", col.names=NA)

#############################################
############################################
###TEST CODE##################################
###################

cam$month = month(cam$dt_close)

map <- qmap('02143', zoom=12, maptype = 'terrain',source ='stamen')

cit <- subset(df, grepl("Citizen", Source))


cit_pts <- geom_point(data = cit, x=cit$LONGITUDE, y = cit$LATITUDE, color = 'orange', alpha = 0.05)
bos_pts <- geom_point(data = bos, x=bos$LONGITUDE, y = bos$LATITUDE, color = 'black', alpha = 0.05)
cam_pts <- geom_point(data = cam, x=cam$LONGITUDE, y = cam$LATITUDE, color = "blue", alpha = 0.05)

map + bos_pts + cam_pts

cam2$lag = as.numeric(difftime(cam2$dt_close, cam2$dt_open, units="days"))
bos2$lag = as.numeric(difftime(bos2$dt_close, bos2$dt_open, units="days"))


p = ggplot(cam2, aes(x = lag)) + 
  geom_histogram(aes(y=..density..),
                 color = "black", fill = "white")  +
  geom_density(alpha = .2, fill = "#FF6666") 

bplot = ggplot(bos2, aes(x = lag)) + 
  geom_histogram(aes(y=..density..),
                 color = "black", fill = "white")  +
  geom_density(alpha = .2, fill = "#FF6666") 

# pot.pts <- SpatialPointsDataFrame(coords,bos[10], proj4string = CRS("+init=epsg:4326"))
# plot(pot.pts, pch = ".", col="light blue")

# cam_coord <- cbind(Longitude = cam$LONGTIDUE, Latitude = cam$LATITUDE)
# bos_coords <- cbind(Longitude = as.numeric(as.character(bos$LONGITUDE)), Latitude = as.numeric(as.character(bos$LATITUDE)))



