########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     geo_diary_addl.R
# Date:     2023 Spring
# About:    Script to geocode diary surveys
########################################

########################################
# Notes

# Run this script from within geo_diary.R script
# Do not run this script on its own
# (unless doing line-by-line testing 
# after running geo_diary.R script)

########################################

# fix missing/errors
# - NOTE: significant amount of code removed for confidentiality
# - example: place[place$HHCODE==1001 & place$PERCODE==1 & place$DATE=="2019-01-30" & place$PLANO==2 & place$PLACE=="Other place", c("PLOC")] <- list("Main St & Center St, Logan, UT")
# - example: place[place$HHCODE==1001 & place$PERCODE==1 & place$DATE=="2019-01-30" & place$PLANO==2, c("PLOC_APPROX","PLOC")] <- list(T,"Main St & Center St, Logan, UT")

# fix missing/school
# - investigated age of adults, travel times on school trips, local schools, bell schedules
logan <- "Logan City Hall, Logan, UT"
usuloc <- "USU, Old Main Hill, Logan, UT"
# - NOTE: significant amount of code removed for confidentiality
# - example: place[place$HHCODE==1001 & place$PERCODE==1 & place$DATE=="2019-01-30" & place$PLANO==2, c("PLOC_APPROX","PLOC")] <- list(T,usuloc)
rm(logan, usuloc)
# - help
# HH[HH$HHCODE==1001,]
# PER[PER$HHCODE==1001,]
# DIARY[DIARY$HHCODE==1001 & !is.na(DIARY$HHCODE),]
# View(place[place$HHCODE==1001,])
# View(PLACE[PLACE$HHCODE==1001,])
# View(TRIP[TRIP$HHCODE==1001,])
# place[place$HHCODE==1001 & place$PERCODE==1 & place$DATE=="2019-01-30" & place$PLANO==2, c("PLOC_APPROX","PLOC")] <- list(T,"")
# - couldn't fix
# - NOTE: code removed for confidentiality
# - example: place[place$HHCODE==1001 & place$PERCODE==1 & place$DATE=="2019-01-30" & place$PLANO==2, c("PLOC_APPROX","PLOC")]

########################################
# Return to geo_diary.R script
########################################