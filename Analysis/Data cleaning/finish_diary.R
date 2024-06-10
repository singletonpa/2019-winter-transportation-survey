########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     finish_diary.R
# About:    Script to finish diary surveys
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
library("sf")
library("mapview")

########################################
# Load data

# Read rds data files
t_folder <- "Data 3a Formatted"
PLACE <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "PLACE.rds"))
TRIP <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "TRIP.rds"))
place <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "geo_place.rds"))
trip <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "geo_trip.rds"))
rm(t_folder)

# Save temp versions
xPLACE <- PLACE
xTRIP <- TRIP
xplace <- place
xtrip <- trip

########################################
# Process census block groups

# Load shapefiles
cbgut <- st_read(dsn=file.path("Data", "GIS", "Census Block Groups", "tl_2019_49_bg"), layer="tl_2019_49_bg", stringsAsFactors=F)
cbgid <- st_read(dsn=file.path("Data", "GIS", "Census Block Groups", "tl_2019_16_bg"), layer="tl_2019_16_bg", stringsAsFactors=F)

# Inspect
# mapview(cbgut) + mapview(cbgid)
# mapview(cbgut[cbgut$COUNTYFP=="005",]) + mapview(cbgid[cbgid$COUNTYFP=="041",])
str(cbgut); str(cbgid)
st_crs(cbgut); st_crs(cbgid)

# Merge, simplify
cbg <- rbind(cbgut, cbgid)
cbg <- cbg[,c("GEOID")]

########################################
# Add CBG to place

# Create spatial: place
place[,c("LAT", "LNG")] <- place[,c("PLOC_LAT", "PLOC_LNG")]
place$LAT[is.na(place$LAT)] <- 0
place$LNG[is.na(place$LNG)] <- 0
place <- st_as_sf(place, crs=st_crs(cbgut), coords=c("LNG", "LAT"))
mapview(cbgut[cbgut$COUNTYFP=="005",]) + mapview(place, col.regions="orange")

# Add geoid: place
names(cbg)[1] <- "PLOC_GEOID"
place <- st_join(place, cbg)
table(place$PLOC_GEOID); table(is.na(place$PLOC_GEOID))
place$geometry <- NULL

# Append geocoded location information: place
str(PLACE)
str(place)
place <- place[,c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "DIARY", "DATE", "PLANO", 
                  "PLOC0", "PLOC", "PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID", "PLOC_APPROX", "PLOC_GEOID")]
PLACE <- merge(PLACE, place, all=T, 
               by=c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "DIARY", "DATE", "PLANO"))
mycols <- c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "PERNAME", "DIARY", "DATE", "PLANO", 
            grep("PLACE", names(PLACE), value=T), grep("PLOC", names(PLACE), value=T), 
            grep("ARR", names(PLACE), value=T), "ACTIVITY", "ACTIVITY_TEXT", "LEAVE", "NOTRIPS", "NOTRIPS_TEXT", 
            grep("DEP", names(PLACE), value=T), "ACT_TIME", "MODE", "MODE_TEXT", "PEOPLE", "VEHICLE", "VEHICLE_TEXT")
PLACE <- PLACE[,mycols]
rm(mycols)
PLACE <- PLACE[order(PLACE$HHCODE, PLACE$PERCODE, PLACE$DATE, PLACE$PLANO),]
row.names(PLACE) <- NULL

# Inspect
str(PLACE); summary(PLACE)
summary(is.na(PLACE$PLOC_GEOID))

########################################
# Append geocoded location information

# Add spatial from PLACE (PLOC1)
tc1 <- c("ResponseId","HHResponseId","HHCODE","PERCODE","PERNAME","DIARY","DATE","PLANO1","PLACE1","ACTIVITY1","PLOC1","PLOC1_PID")
tc2 <- c("ResponseId","HHResponseId","HHCODE","PERCODE","PERNAME","DIARY","DATE","PLANO2","PLACE2","ACTIVITY2","PLOC2","PLOC2_PID")
tcp <- c("ResponseId","HHResponseId","HHCODE","PERCODE","PERNAME","DIARY","DATE","PLANO","PLOC_LAT","PLOC_LNG","PLOC_APPROX","PLOC_GEOID")
temp1 <- merge(trip[,tc1], PLACE[,c(tcp)], by.x=tc1[1:8], by.y=tcp[1:8], all.x=T, all.y=F)
names(temp1)[grepl("PLOC_", names(temp1))] <- gsub("PLOC_", "PLOC1_", names(temp1)[grepl("PLOC_", names(temp1))])
temp2 <- merge(trip[,tc2], PLACE[,c(tcp)], by.x=tc2[1:8], by.y=tcp[1:8], all.x=T, all.y=F)
names(temp2)[grepl("PLOC_", names(temp2))] <- gsub("PLOC_", "PLOC2_", names(temp2)[grepl("PLOC_", names(temp2))])
rm(tc1, tc2, tcp)

# Re-create trip
tc1 <- c("ResponseId","HHResponseId","HHCODE","PERCODE","PERNAME","DIARY","DATE","PLANO1","PLOC1_LAT","PLOC1_LNG","PLOC1_APPROX","PLOC1_GEOID")
tc2 <- c("ResponseId","HHResponseId","HHCODE","PERCODE","PERNAME","DIARY","DATE","PLANO2","PLOC2_LAT","PLOC2_LNG","PLOC2_APPROX","PLOC2_GEOID")
temp <- trip
temp <- merge(temp, temp1[tc1], by=tc1[1:8])
temp <- merge(temp, temp2[tc2], by=tc2[1:8])
rm(tc1, tc2)

# Re-arrange trip
tc1 <- names(temp1)
tc2 <- c("DEP_TIME", "MODE", "PEOPLE", "VEHICLE")
tc3 <- names(temp2)[!(names(temp2) %in% names(temp1))]
tc4 <- names(trip)[!names(trip) %in% c(tc1, tc2, tc3)]
tc <- c(tc1, tc2, tc3, tc4)
temp <- temp[,tc]
rm(tc1, tc2, tc3, tc4, tc)
TRIP <- temp[order(temp$HHCODE, temp$PERCODE, temp$DATE, temp$PLANO1),]
row.names(temp) <- NULL

# Inspect
str(TRIP); summary(TRIP)
summary(is.na(TRIP$PLOC1_GEOID))
summary(is.na(TRIP$PLOC2_GEOID))

# Cleanup
rm(temp, temp1, temp2)

########################################
# Save

# Save
t_folder <- "Data 3b Finished"
saveRDS(PLACE, file=file.path("Data", "Survey 2019 Winter", t_folder, "PLACE.rds"))
write.csv(PLACE, file=file.path("Data", "Survey 2019 Winter", t_folder, "PLACE.csv"), row.names=F)
saveRDS(TRIP, file=file.path("Data", "Survey 2019 Winter", t_folder, "TRIP.rds"))
write.csv(TRIP, file=file.path("Data", "Survey 2019 Winter", t_folder, "TRIP.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(cbg, cbgut, cbgid)
rm(PLACE, TRIP, place, trip)
rm(xPLACE, xTRIP, xplace, xtrip)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################