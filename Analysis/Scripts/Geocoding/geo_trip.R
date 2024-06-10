########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     geo_trip.R
# About:    Script to geocode trips from travel diary surveys
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library("")

########################################
# Load data

# Load data
# Load diary, place, trip information
DIARY <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "DIARY.rds"))
PLACE <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PLACE.rds"))
TRIP <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "TRIP.rds"))
# load place locations
place <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "geo_place.rds"))

########################################
# Re-create trip for geocoding

# Pick column mames
mycols0 <- c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "PERNAME", "DIARY", "DATE")
mycols1 <- c("PLANO", "PLACE", "ACTIVITY", "PLOC", "PLOC_PID", "DEP_TIME")
mycols2 <- c("PLANO", "PLACE", "ACTIVITY", "PLOC", "PLOC_PID", "ARR_TIME")
mycols3 <- c("MODE", "PEOPLE", "VEHICLE")

# Initialize places
mycolsu <- unique(c(mycols0, mycols1, mycols2, mycols3))
tc1 <- mycolsu[mycolsu %in% names(place)]
tc2 <- mycolsu[mycolsu %in% names(PLACE)]
tc3 <- tc1[tc1 %in% tc2]
places <- merge(place[,tc1], PLACE[,tc2], by=c(tc3))
places <- places[order(places$HHCODE, places$PERCODE, places$DATE, places$PLANO),]
row.names(places) <- NULL
rm(mycolsu, tc1, tc2, tc3)

# Initialize trip
trr <- unique(places$ResponseId)
i <- trr[1]
t0 <- places[places$ResponseId==i,]
t1 <- t0[1:(nrow(t0)-1),c(mycols0, mycols1, mycols3)]
names(t1)[names(t1) %in% mycols1] <- c("PLANO1", "PLACE1", "ACTIVITY1", "PLOC1", "PLOC1_PID", "DEP_TIME")
t1 <- cbind(t1, t0[2:nrow(t0),c(mycols2)])
names(t1)[names(t1) %in% mycols2] <- c("PLANO2", "PLACE2", "ACTIVITY1", "PLOC2", "PLOC2_PID", "ARR_TIME")
temp <- t1[0,]
rm(trr, i, t0, t1)

# For loop
for (i in unique(places$ResponseId)) {
  t0 <- places[places$ResponseId==i,]
  t0 <- t0[order(t0$PLANO),]
  if (nrow(t0)>=2) {
    t1 <- t0[1:(nrow(t0)-1),c(mycols0, mycols1, mycols3)]
    names(t1)[names(t1) %in% mycols1] <- c("PLANO1", "PLACE1", "ACTIVITY1", "PLOC1", "PLOC1_PID", "DEP_TIME")
    t1 <- cbind(t1, t0[2:nrow(t0),c(mycols2)])
    names(t1)[names(t1) %in% mycols2] <- c("PLANO2", "PLACE2", "ACTIVITY2", "PLOC2", "PLOC2_PID", "ARR_TIME")
    temp <- rbind(temp, t1)
    rm(t1)
  }
  rm(t0)
}; rm(i)

# Reorganize
temp <- temp[order(temp$HHCODE, temp$PERCODE, temp$DATE, temp$PLANO1),]
row.names(temp) <- NULL

# Create time
temp$TRIP_TIME <- difftime(temp$ARR_TIME, temp$DEP_TIME, units="mins")
summary(as.numeric(temp$TRIP_TIME))

########################################
# Prepare geocoding stuff

# Resources
# https://cran.r-project.org/web/packages/ggmap/
# https://towardsdatascience.com/breaking-down-geocoding-in-r-a-complete-guide-1d0f8acd0d4b

# Load package
# install.packages(ggmap)
library(ggmap)

# Load API key
apikey.geocoding <- scan(file.path("Analysis", "Scripts", "Geocoding", "apikey.geocoding.txt"), what="character")

# Register API key
register_google(key=apikey.geocoding)

# Remove API key
rm(apikey.geocoding)

# # Test functions in ggmap package
# # using route()
# route_df <- route(from="houson, texas", to="waco, texas", mode="driving", output="simple")
# route_df <- apply(route_df, 1, function(x) {
#   v <- as.numeric(x[c(7,9,8,10)])
#   m <- matrix(v, nrow = 2)
#   return(st_sfc(st_linestring(m), crs=4326))
# })
# mapview(route_df)
# # - don't use, not fine-grained enough
# rm(route_df)
# # using trek()
# trek_df <- trek(from="houson, texas", to="waco, texas", mode="driving", output="simple")
# trek_df <- st_as_sf(trek_df, coords=c("lon", "lat"), crs=4326)
# mapview(trek_df)
# # - maybe use, would need complete output
# trek_df <- trek(from="houson, texas", to="waco, texas", mode="driving", output="all")
# # - don't use, too complicated to parse
# rm(trek_df)
# # using mapdist()
# mapdist(from="houson, texas", to="waco, texas", mode="driving", output="simple")
# mapdist(from="houson, texas", to="waco, texas", mode="walking", output="simple")
# mapdist(from="houson, texas", to="waco, texas", mode="bicycling", output="simple")
# mapdist(from="houson, texas", to="waco, texas", mode="transit", output="simple")
# # - yes, use this, easy to extract information

# Function to get geocoded trip information
get_geotrip <- function(tpid1, tpid2, deptime) {
  if (is.na(tpid1) | is.na(tpid2)) {
    # return
    res <- list(NA, NA, NA, NA, NA, NA, NA, NA, "")
    return(res)
  } else {
    # process place IDs
    t1 <- paste0("place_id:", tpid1)
    t2 <- paste0("place_id:", tpid2)
    # process time to use departure time
    thr <- format(deptime, "%H")
    tmin <- format(deptime, "%M")
    tday <- Sys.time()
    twkd <- as.integer(format(tday, "%w"))
    if (twkd > 3) { # find next Wednesday
      tadd <- 3L - twkd + 7L
    } else { tadd <- 3L - twkd }
    tday <- format(tday, "%Y-%m-%d")
    ttime <- as.POSIXct(paste0(tday, " ", thr, ":", tmin, ":00"), tz="America/Denver")
    ttime <- ttime + tadd*24*60*60
    rm(thr, tmin, tday, twkd, tadd)
    # geocode trip distances and travel times
    timedep <- paste0("departure_time=", as.integer(ttime))
    tgeoD <- mapdist(from=t1, to=t2, mode="driving", output="all", inject=timedep)
    tgeoW <- mapdist(from=t1, to=t2, mode="walking", output="all", inject=timedep)
    tgeoB <- mapdist(from=t1, to=t2, mode="bicycling", output="all", inject=timedep)
    tgeoT <- mapdist(from=t1, to=t2, mode="transit", output="all", inject=timedep)
    # check modes used for transit
    tmodes <- c()
    tt <- trek(from=t1, to=t2, mode="transit", output="all", inject=timedep)
    if (length(tt)>0) {
      if (tt$status != "NOT_FOUND") {
        for (z in 1:length(tt$routes)) {
          tr <- tt$routes[[z]]
          for (y in 1:length(tr$legs)) {
            tl <- tr$legs[[y]]
            for (x in 1:length(tl$steps)) {
              ts <- tl$steps[[x]]
              tmodes <- c(tmodes, ts$travel_mode)
              rm(ts)
            }; rm(x)
            rm(tl)
          }; rm(y)
          rm(tr)
        }; rm(z)
      }
    }
    rm(tt)
    # extract information
    # distance in meters -> miles
    m2mi <- 1/1609.344
    disD <- tgeoD[[1]][[1]]$distance$value * m2mi
    disW <- tgeoW[[1]][[1]]$distance$value * m2mi
    disB <- tgeoB[[1]][[1]]$distance$value * m2mi
    disT <- tgeoT[[1]][[1]]$distance$value * m2mi
    rm(m2mi)
    # duration in seconds
    s2m <- 1/60
    durD <- tgeoD[[1]][[1]]$duration$value * s2m
    durW <- tgeoW[[1]][[1]]$duration$value * s2m
    durB <- tgeoB[[1]][[1]]$duration$value * s2m
    durT <- tgeoT[[1]][[1]]$duration$value * s2m
    rm(s2m)
    # comments
    text <- ""
    if (!("TRANSIT" %in% tmodes)) {
      disT <- NA; durT <- NA
      text <- "transit_not_in_trek"
    }
    # check if empty
    if (length(disD)==0) { disD <- NA }
    if (length(disW)==0) { disW <- NA }
    if (length(disB)==0) { disB <- NA }
    if (length(disT)==0) { disT <- NA }
    if (length(durD)==0) { durD <- NA }
    if (length(durW)==0) { durW <- NA }
    if (length(durB)==0) { durB <- NA }
    if (length(durT)==0) { durT <- NA }
    # return
    res <- list(disD, disW, disB, disT, durD, durW, durB, durT, text)
    return(res)
    # cleanup
    rm(t1, t2, timedep, tgeoD, tgeoW, tgeoB, tgeoT, ttime, tmodes)
    rm(res, disD, disW, disB, disT, durD, durW, durB, durT, text)
  }
  # cleanup
  rm(tpid1, tpid2, deptime)
}

# # Test
# get_geotrip(place$PLOC_PID[1], place$PLOC_PID[2], Sys.time())
# get_geotrip(place$PLOC_PID[1], place$PLOC_PID[2], TRIP$DEP_TIME[1])
# get_geotrip(place$PLOC_PID[1], place$PLOC_PID[5], TRIP$DEP_TIME[4])
# get_geotrip(place$PLOC_PID[1], place$PLOC_PID[12], TRIP$DEP_TIME[10])
# get_geotrip(temp$PLOC1_PID[1], temp$PLOC2_PID[1], temp$DEP_TIME[1])
# get_geotrip(temp$PLOC1_PID[4], temp$PLOC2_PID[4], temp$DEP_TIME[4])
# get_geotrip(temp$PLOC1_PID[10], temp$PLOC2_PID[10], temp$DEP_TIME[10])

########################################
# Geocode trips

# Initialize columns for geocoding
tcols <- c(paste("DIST", c("D","W","B","T"), sep="_"), paste("TIME", c("D","W","B","T"), sep="_"), "TEXT_T")
temp[,tcols[1:8]] <- 0
temp[,tcols[9]] <- ""

# Geocode
# - do in batches of 1000, to check that it works
# for (i in 1:100) {
# for (i in 101:1000) {
# for (i in 1001:2000) {
# for (i in 2001:3000) {
# for (i in 3001:4000) {
# for (i in 4001:5000) {
# for (i in 5001:6000) {
# for (i in 6001:7000) {
# for (i in 7001:8000) {
# for (i in 8001:nrow(temp)) {
for (i in 1:nrow(temp)) {
  # get geocoded trip information
  tgeo <- get_geotrip(temp$PLOC1_PID[i], temp$PLOC2_PID[i], temp$DEP_TIME[i])
  # assign geocode
  temp[i,tcols] <- tgeo
  # remove
  rm(tgeo)
}; rm(i)

# Initial inspection
summary(temp[1:nrow(temp),tcols[1:9]])
table(temp$MODE, is.na(temp$DIST_T))

# Function to get geocoded information (places)
get_geo <- function(tloc) {
  # geocode
  tgeo <- geocode(location=tloc, output="all", source="google")
  if ("results" %in% names(tgeo)) {
    # assemble results
    tlat <- tgeo$results[[1]]$geometry$location$lat
    tlng <- tgeo$results[[1]]$geometry$location$lng
    tadd <- tgeo$results[[1]]$formatted_address
    tpid <- tgeo$results[[1]]$place_id
    # return
    return(list(tlat, tlng, tadd, tpid))
  } else {
    return(list(NA, NA, NA, NA))
  }
}

# Deal with missing DIST/TIME
tr <- which(rowSums(is.na(temp[,tcols[1:8]]))==8)
View(temp[tr,])
# - NOTE: code removed for confidentiality
# - example: tr <- tr[tr %in% which(temp$HHCODE %in% c(1001))]
# didn't get any trip information assigned to them, why? 
# - appears that the PID from Google is no longer good
# i <- tr[1]
# get_geotrip(temp$PLOC1_PID[i], temp$PLOC2_PID[i], temp$DEP_TIME[i])
# get_geo(temp[i,"PLOC2"])
# - NOTE: code removed for confidentiality
# - example: HHCODE==1001
# - example: tr <- which(rowSums(is.na(temp[,tcols[1:8]]))==8 & temp$HHCODE==1001)
# - example: i <- tr[1]; temp[i,]
# - example: temp[i,c("PLOC2", "PLOC2_PID")]
# - example: tgeo <- get_geo("Main St & Center St, Logan, UT"); tgeo
# - example: temp[tr,"PLOC1_PID"][which(temp[tr,"PLOC1"]=="Main St & Center St, Logan, UT")] <- tgeo[[4]]
# - example: temp[tr,"PLOC2_PID"][which(temp[tr,"PLOC2"]=="Main St & Center St, Logan, UT")] <- tgeo[[4]]
# - example: temp[tr,]
# - example: rm(tr, i)
# re-geocode
tr <- which(rowSums(is.na(temp[,tcols[1:8]]))==8)
# - NOTE: code removed for confidentiality
# - example: tr <- tr[tr %in% which(temp$HHCODE %in% c(1001))]
for (i in tr) {
  # get geocoded trip information
  tgeo <- get_geotrip(temp$PLOC1_PID[i], temp$PLOC2_PID[i], temp$DEP_TIME[i])
  # assign geocode
  temp[i,tcols] <- tgeo
  # remove
  rm(tgeo)
}; rm(i)
# inspect
View(temp[tr,])
rm(tr)

# Additional inspection
summary(temp[1:nrow(temp),tcols[1:9]])
table(temp$MODE, is.na(temp$DIST_T))

########################################
# Process by chosen mode

# Inspect
table(temp$MODE, is.na(temp$TIME_D))
table(temp$MODE, is.na(temp$TIME_W))
table(temp$MODE, is.na(temp$TIME_B))
table(temp$MODE, is.na(temp$TIME_T))

# Create distance and time by mode
temp$DIST <- NA
temp$TIME <- NA
temp[!is.na(temp$MODE) & temp$MODE=="Walk",c("DIST", "TIME")] <- temp[!is.na(temp$MODE) & temp$MODE=="Walk",c("DIST_W", "TIME_W")]
temp[!is.na(temp$MODE) & temp$MODE=="Bicycle",c("DIST", "TIME")] <- temp[!is.na(temp$MODE) & temp$MODE=="Bicycle",c("DIST_B", "TIME_B")]
temp[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Driver",c("DIST", "TIME")] <- temp[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Driver",c("DIST_D", "TIME_D")]
temp[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Passenger",c("DIST", "TIME")] <- temp[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Passenger",c("DIST_D", "TIME_D")]
temp[!is.na(temp$MODE) & temp$MODE=="Motorcycle/Scooter/Moped",c("DIST", "TIME")] <- temp[!is.na(temp$MODE) & temp$MODE=="Motorcycle/Scooter/Moped",c("DIST_D", "TIME_D")]
temp[!is.na(temp$MODE) & temp$MODE=="Local Bus (CVTD or Aggie Shuttle)",c("DIST", "TIME")] <- temp[!is.na(temp$MODE) & temp$MODE=="Local Bus (CVTD or Aggie Shuttle)",c("DIST_T", "TIME_T")]
# temp[!is.na(temp$MODE) & temp$MODE=="School Bus",c("DIST", "TIME")] <- temp[!is.na(temp$MODE) & temp$MODE=="School Bus",c("DIST_D", "TIME_D")]
# temp[!is.na(temp$MODE) & temp$MODE=="Other (please specify)",c("DIST", "TIME")] <- temp[!is.na(temp$MODE) & temp$MODE=="Other (please specify)",c("DIST_D", "TIME_D")]

# Plot travel distance distribution by mode
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Walk"], 1000)
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Walk"], 1000, xlim=c(0,5))
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Bicycle"], 100)
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Driver"], 1000)
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Driver"], 1000, xlim=c(0,100))
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Passenger"], 1000)
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Passenger"], 1000, xlim=c(0,100))
# hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Motorcycle/Scooter/Moped"], 100)
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Local Bus (CVTD or Aggie Shuttle)"], 1000)
hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Local Bus (CVTD or Aggie Shuttle)"], 1000, xlim=c(0,25))
# hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="School Bus"], 100)
# hist(temp$DIST[!is.na(temp$MODE) & temp$MODE=="Other (please specify)"], 100)

# Plot concordance of self-report vs. Google Maps travel time by mode
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Walk"], 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Walk"])
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Walk"], pch=20, col=alpha("black",0.25), 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Walk"], xlim=c(0,120), ylim=c(0,120))
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Bicycle"], 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Bicycle"])
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Bicycle"], pch=20, col=alpha("black",0.25), 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Bicycle"], xlim=c(0,120), ylim=c(0,120))
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Driver"], 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Driver"])
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Driver"], pch=20, col=alpha("black",0.25), 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Driver"], xlim=c(0,240), ylim=c(0,240))
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Passenger"], 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Passenger"])
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Passenger"], pch=20, col=alpha("black",0.25), 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Car/Van/Truck/SUV Passenger"], xlim=c(0,240), ylim=c(0,240))
# plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Motorcycle/Scooter/Moped"], 
#      temp$TIME[!is.na(temp$MODE) & temp$MODE=="Motorcycle/Scooter/Moped"])
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Local Bus (CVTD or Aggie Shuttle)"], 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Local Bus (CVTD or Aggie Shuttle)"])
plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Local Bus (CVTD or Aggie Shuttle)"], pch=20, col=alpha("black",0.25), 
     temp$TIME[!is.na(temp$MODE) & temp$MODE=="Local Bus (CVTD or Aggie Shuttle)"], xlim=c(0,120), ylim=c(0,120))
# plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="School Bus"], 
#      temp$TIME[!is.na(temp$MODE) & temp$MODE=="School Bus"])
# plot(temp$TRIP_TIME[!is.na(temp$MODE) & temp$MODE=="Other (please specify)"], 
#      temp$TIME[!is.na(temp$MODE) & temp$MODE=="Other (please specify)"])

# Save
# saveRDS(temp, file.path("temptrip.rds"))
# temp <- readRDS(file.path("temptrip.rds"))
saveRDS(temp, file.path("Data", "geo_trip.rds"))
# geocoded on 2023-03-05 & 2023-03-06

########################################
# Cleanup

# Remove
rm(mycols0, mycols1, mycols2, mycols3, tcols)
rm(DIARY, PLACE, TRIP)
rm(place, places, temp)
rm(get_geo, get_geotrip)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################