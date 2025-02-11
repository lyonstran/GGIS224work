#Parks in Chicago

#Go through this script as an example of data preparation that will enable 
#a more in-depth analysis of park significance to public health. 

#GGIS, UIUC

################################################################################
#Load packages for this exercise
library(sf)
library(tidyverse)
library(tmap)
library(dplyr) #Load data wrangling library
################################################################################

#Define your directory that contains the Parks.geojson data
setwd("C:/Users/lyons/OneDrive/Desktop/ggis224/ChicagoParks")

#Read Parks.geojson (parks in Chicago)
chiparks <- st_read("Parks.geojson")

#Map parks as points
tm_shape(chiparks) + tm_dots()

#Please explore what attributes chiparks provide. 

################################################################################


#Read in Chicago tract level data (chives-data.geojson) from Lab 3
chitracts <- st_read("Parks.geojson") #Enter your directory

#Recall from Lab3:
#The Chives dataset was downloaded from chichives.com. 
#ChiVes is a collaborative project that integrates and visualizes data; 
#a handful of key metrics―tree cover, air pollution estimates, heat island effects, 
#traffic volumes, and social vulnerability index― help to reveal where in the city 
#people face particular challenges as we work towards a healthier Chicago. 
#You will need to review the Data page to understand the variable names and definitions.
#Please explore what attributes are available in chitracts. 

#Map census tracts and parks to confirm they overlap
tm_shape(chitracts) + tm_fill() + 
  tm_shape(chiparks) + tm_dots(alpha = 0.4) 

#Prepare data for a spatial data operation => Check CRS
st_crs(chiparks)
st_crs(chitracts)

#Transform to CRS of chiparks
#chitracts <- st_transform(chitracts, st_crs(chiparks))

#Check data attributes and contents
head(chiparks)
head(chitracts)

#Query the count of pakrs and the count of census tracts
nrow(chiparks) #4467 park points
nrow(chitracts) #801 tracts

#Spatially join parks with tracts; data remains as points/parks.
pipr = st_join(chiparks, chitracts) #joining chitracts (tract) attributes to chiparks (parks, the target)
head(pipr)

#Identify which column will be our Tract ID within points
head(chitracts) #geoid is the tract ID

#Aggregate count of parks by GEOID.
ptcount = as.data.frame(table(pipr$geoid))
head(ptcount)
nrow(ptcount)

#Rename column fields to make for easy merging back to Tracts file
names(ptcount) = c("geoid", "ParkCt")
head(ptcount)

#Merge count variable back to tracts
areas = merge(chitracts, ptcount, by="geoid", all = TRUE)
head(areas)

#Map tracts by the number of parks
tm_shape(areas) + tm_fill("ParkCt", style = "jenks", n=6, alpha = 0.8)

#Merge data to community areas as new spatial variables
ParkCom = areas %>% 
  group_by(community) %>%
  summarize(Parks = sum(ParkCt, na.rm = TRUE),
            TotTracts = n())

#Map new spatial variable (i.e., Parks) for each of the communities
tm_shape(ParkCom) + tm_fill("Parks", style = "jenks",
                            pal = "BuPu", n=6, alpha = 0.8)


# For your interest, feel free to try the extended exercises below:
#Count the number of parks by tract in Chicago
#Develop a 1-mile park buffer in Chicago
#Count the number of park buffers by tract in Chicago (what does this operation signify?)