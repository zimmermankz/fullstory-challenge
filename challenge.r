library(dplyr)

#load data
trip.data <- read.csv("yellow_tripdata_2017-06.csv")
zone.lookup <- read.csv("taxi+_zone_lookup.csv")


######## create additional columns ########
#find length of trip (trip time will be in seconds)
trip.data$tpep_dropoff_datetime <- as.POSIXct(as.character(trip.data$tpep_dropoff_datetime), format="%Y-%m-%d %H:%M:%S")
trip.data$tpep_pickup_datetime <- as.POSIXct(as.character(trip.data$tpep_pickup_datetime), format="%Y-%m-%d %H:%M:%S")
trip.data$trip_time <- as.numeric(trip.data$tpep_dropoff_datetime - trip.data$tpep_pickup_datetime)

#compute income to driver as sum of fare and tips
trip.data$income <- trip.data$fare_amount + trip.data$tip_amount

#compute total income per hour, as well as tips/hour and fare/hour
trip.data$income_per_hour <- (trip.data$income / trip.data$trip_time) * 3600
trip.data$tips_per_hour <- (trip.data$tip_amount / trip.data$trip_time) * 3600
trip.data$fare_per_hour <- (trip.data$fare_amount / trip.data$trip_time) * 3600

#separate out hour as own column (to find best time of day)
trip.data$pickup_hour <- strftime(trip.data$tpep_pickup_datetime, format="%H")
trip.data$day_of_week <- weekdays(trip.data$tpep_pickup_datetime, F)

#remove trips with 0 length, those seem like errors
trip.data <- trip.data[which(trip.data$trip_distance > 0), ]

#some trips are 0 seconds (or other unreasonably low numbers), define minimum length to avoid what seem to be errors
min.trip.time = 30
min.at.location = 30

######## find the best pickup location ########
#by income
income.by.pickup.location <- trip.data[which(trip.data$trip_time >= min.trip.time), c("PULocationID", "income_per_hour", "trip_time")] %>%
  group_by(PULocationID) %>%
  filter(n() >= min.at.location) %>%
  summarise(income_per_hour = weighted.mean(income_per_hour, trip_time))
income.by.pickup.location <- merge(income.by.pickup.location, zone.lookup[,c("LocationID", "Borough", "Zone")], by.x="PULocationID", by.y="LocationID")
income.by.pickup.location[order(-income.by.pickup.location$income_per_hour), ]

#by fare
# fare.by.pickup.location <- trip.data[which(trip.data$trip_time >= min.trip.time), c("PULocationID", "fare_per_hour", "trip_time")] %>%
#   group_by(PULocationID) %>%
#   filter(n() >= min.at.location) %>%
#   summarise(fare_per_hour = weighted.mean(fare_per_hour, trip_time))
# fare.by.pickup.location <- merge(fare.by.pickup.location, zone.lookup[,c("LocationID", "Borough", "Zone")], by.x="PULocationID", by.y="LocationID")
# head(fare.by.pickup.location[order(-fare.by.pickup.location$fare_per_hour), ])

#by tips
# tips.by.pickup.location <- trip.data[which(trip.data$trip_time >= min.trip.time), c("PULocationID", "tips_per_hour", "trip_time")] %>%
#   group_by(PULocationID) %>%
#   filter(n() >= min.at.location) %>%
#   summarise(tips_per_hour = weighted.mean(tips_per_hour, trip_time))
# tips.by.pickup.location <- merge(tips.by.pickup.location, zone.lookup[,c("LocationID", "Borough", "Zone")], by.x="PULocationID", by.y="LocationID")
# head(tips.by.pickup.location[order(-tips.by.pickup.location$tips_per_hour), ])


######## find best pickup time ########
#by income (time only)
income.by.pickup.time <- data.frame(trip.data[which(trip.data$trip_time >= min.trip.time), c("pickup_hour", "income_per_hour", "trip_time")] %>%
  group_by(pickup_hour) %>%
  summarise(income_per_hour = weighted.mean(income_per_hour, trip_time)))
income.by.pickup.time[order(-income.by.pickup.time$income_per_hour), ]

#by income (day of week only)
income.by.pickup.day <- data.frame(trip.data[which(trip.data$trip_time >= min.trip.time), c("day_of_week", "income_per_hour", "trip_time")] %>%
  group_by(day_of_week) %>%
  summarise(income_per_hour = weighted.mean(income_per_hour, trip_time)))
income.by.pickup.day[order(-income.by.pickup.day$income_per_hour), ]

#by income (time and day)
income.by.pickup.day.and.time <- data.frame(trip.data[which(trip.data$trip_time >= min.trip.time), c("pickup_hour", "day_of_week", "income_per_hour", "trip_time")] %>%
  group_by(pickup_hour, day_of_week) %>%
  summarise(income_per_hour = weighted.mean(income_per_hour, trip_time)))
income.by.pickup.day.and.time[order(-income.by.pickup.day.and.time$income_per_hour), ]


######## make some pretty pictures ########
#simple barplot of average income/hr by hour of the day
barplot(income.by.pickup.time$income_per_hour, 
        names.arg = income.by.pickup.time$pickup_hour,
        xlab = "Hour of the Day",
        ylab = "Avg Income/Hr with Passenger in Cab",
        ylim = c(0, 80),
        col = "darkgreen")

### shade map of NYC taxi zones with average income/hr

# NOTE - this will install any packages in the following list that are not installed
needed_packages <- c("rgdal", "leaflet", "RColorBrewer")
for(package in needed_packages) {
  if(!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

#load zone map of NYC -  source for map: https://data.cityofnewyork.us/Transportation/NYC-Taxi-Zones/d3c5-ddgc
new.york.zone.map <- readOGR("NYC Taxi Zones.kml")

#create color palette, bin cutoffs were chosen manually after inspecting the data
palette <- colorBin(brewer.pal(11, "RdYlGn"), bins=c(20, 40, 50, 55, 60, 65, 70, 80, 90, 100, 300, 500))

#prepare to fill out all zones, even ones without sufficient data
incomes.by.location.completed <- merge(income.by.pickup.location, zone.lookup[,c("LocationID", "Borough")], by.x="PULocationID", by.y="LocationID", all.y=T)[1:263,]
incomes.by.location.completed$income_per_hour[is.na(incomes.by.location.completed$income_per_hour)] <- 0

#shade the map and display it
shaded.map <- leaflet() %>%
  addPolygons(data = new.york.zone.map, 
              fillColor = ~palette(incomes.by.location.completed$income_per_hour), 
              fillOpacity = 1,
              weight = 1) %>%
  addLegend(position = "bottomright",
          colors = brewer.pal(11, "RdYlGn"),
          labels = c("$20/hr - $40/hr", "$40/hr - $50/hr", "$50/hr - $55/hr", "$55/hr - $60/hr", "$60/hr - $65/hr", "$65/hr - $70/hr", "$70/hr - $80/hr", "$80/hr - $90/hr", "$90/hr - $100/hr", "$100/hr - $300/hr", "$300/hr - $500/hr"),
          bins = 11,
          opacity = 0.7)
print(shaded.map)


