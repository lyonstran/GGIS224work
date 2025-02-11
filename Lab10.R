# GGIS 224 Lab 10, 04/03/2024
# Emission point analysis

# Instruction:
# Go through this R file in detail and understand the meaning of each script line. 
# Answer the four questions (Q1 to Q4).
# Then organize the entire R file into a R Markdown file. 
# Your R Markdown file should include each of the steps as below, including your answers to the four questions. 



########################
# ENVIRONMENT SET UP
########################

# set working directory
setwd("C:/Users/lyons/OneDrive/Desktop/ggis224")

# load libraries
library(tidyverse)
library(sf)
library(tmap)
library(leaflet)



########################
# DATA
########################

# COUNTY BOUNDARY POLYGON DATA
# 2018 Illinois County Boundaries
# Source: Census, https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
counties.original <- st_read("Data/County Boundaries/cb_2018_us_county_500k.shp")
head(counties.original) # KEY = GEOID is 5-digit county ID 

# Subset to just IL Data
counties.IL <- counties.original |> filter(STATEFP == 17) 
plot(counties.IL)

# ILLINOIS STATE BOUNDARY
il <- st_union(counties.IL) 
plot(il)

# COUNTY HEALTH ATTRIBUTE DATA
# 2023 Length of Life Estimates 
# Source: County Health Rankings, https://www.countyhealthrankings.org/explore-health-rankings/illinois/data-and-resources
# Technical Documentation: https://www.countyhealthrankings.org/sites/default/files/media/document/2023%20CHRR%20Technical%20Document.pdf
# Length of Life RANK = lower scores indicate BEST health, higher scores indicate WORSE health
lengthLife <- read.csv("Data/IL_LengthLife_CHR.csv")
head(lengthLife) #FIPS = County Key

# Merge Health Data to master county file
counties <- merge(counties.IL, lengthLife, by.x="GEOID", by.y="FIPS") #Essential a table join. 
head(counties) #Didn't work; inspect...CO_FIPS a bit strange!

# Quick map for confirmation
# Length of Life RANK = lower scores indicate BETTER health, higher scores indicate WORSE health
tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties) + tm_polygons("LLRank", palette = "BuPu") +  
  tm_layout(frame = FALSE, legend.outside = TRUE)

# ADDING EMISSIONS POINT DATA
# 2020 National Emissions Inventory, Facility Data in IL
# Source: EPA, https://www.epa.gov/air-emissions-inventories/2020-national-emissions-inventory-nei-data

emissions.original <- read.csv("Data/NEI2020_FacilityIL.csv")
head(emissions.original)

#Convert CSV to Spatial Data
emissions <- st_as_sf(emissions.original, 
                      coords = c("Longitude", "Latitude"),
                      crs = 4326)

# Check for type of pollutant in the dataset
unique(emissions$Pollutant)



# Let's map some dot maps below.

########################
# GRADUATED COLOR MAP
########################

# Using dots, adjust style, palette, and number of bins (n)
# By now, you are expected to be able to self-interpret what following statement does.
tm_shape(il) + tm_borders(lwd = 2) + #Draw state boundary
  tm_shape(counties) + tm_borders(lwd = 0.5) +  #overlay county boundaries
  tm_shape(emissions) +  #overlay point coordinates
  tm_dots("Emissions..Tons.", palette = "BrBG", #then apply graduated color based on attribute "Emissions..Tons."
          n = 10, style = "quantile",
          title="Lead Emissions (tons)") + 
  tm_layout(frame = FALSE, legend.outside = TRUE)



########################
# GRADUATED SYMBOLOGY MAP
########################

# Using bubbles, adjust size, color, style, and number of bins (n)
# Again, you are expected to be able to self-interpret what following statement does.
tm_shape(il) + tm_borders(lwd = 2) + #Draw state boundary
  tm_shape(counties) + tm_borders(lwd = 0.5) + #overlay county boundaries
  tm_shape(emissions) + #overlay point coordinates
  tm_bubbles("Emissions..Tons.", col = "tomato1") + #then apply graduated symbols based on attribute "Emissions..Tons."
  tm_layout(frame = FALSE, legend.outside = TRUE)



########################
# GRADUATED SYMBOLOGY MAP WITH CHOROPLETH MAP OVERLAY
######################## 

# Add the health data for exploration of associations with lead emissions.
tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties) + tm_polygons("LLRank", palette = "Greys") +  
  tm_shape(emissions) + 
  tm_bubbles(size = "Emissions..Tons.",  col = "Emissions..Tons.", 
             palette = "PuRd", style = "quantile") + 
  tm_layout(frame = FALSE, legend.outside = TRUE) 



########################
# INTERACTIVE MAP
######################## 

tmap_mode("view")

# Be sure to "Zoom" to explore map interactively
tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties) + tm_polygons("LLRank", alpha = 0.5, palette = "Greys") +  
  tm_shape(emissions) + 
  tm_bubbles(size = "Emissions..Tons.",  col = "Emissions..Tons.", 
             palette = "PuRd", style = "quantile") +  
  tm_basemap("Esri.WorldTopoMap")

# Search for alternate basemaps -- 
# Use Leaflet Provider Demo at https://leaflet-extras.github.io/leaflet-providers/preview/
# & try out different Provider Names (in the tm_basemap argument)
tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties) + tm_polygons("LLRank", alpha = 0.5, palette = "Greys") +  
  tm_shape(emissions) + 
  tm_bubbles(size = "Emissions..Tons.",  col = "Emissions..Tons.", 
             palette = "PuRd", style = "quantile") +  
  tm_basemap("OpenStreetMap.HOT") 

# Switch back to plot mode
tmap_mode("plot")



# Let's do more analysis.

########################
# Re-Transform for Spatial Operations
########################
st_crs(emissions)
st_crs(counties)

emissions.3435 <- st_transform(emissions, 3435) #"NAD83 / Illinois East (ftUS)"
counties.3435 <- st_transform(counties, 3435)

st_crs(emissions.3435)
st_crs(counties.3435) #102 counties



########################
# CALCULATE FACILITY COUNT BY COUNTY
########################
nrow(emissions.3435) #850 emission facility points
nrow(counties.3435) #102 counties

# Count all of the facilities that intersect a county.
# we then use lengths() to find out how many items are present in a vector (i.e. county).
# Then, we store the facility counts for each of the counties into a new attribute called TotEmisFac.
counties.3435$TotEmisFac <- lengths(st_intersects(counties.3435, emissions.3435))

# Map the facility count per county as a choropleth map
tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties.3435) + tm_polygons(col = "TotEmisFac", n=10,
                                    style = "jenks", title = "Total Facilities") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# Calculate County Area
counties.3435$county_area <- st_area(counties.3435) 


#======================================================================================
# Q1: 
# What is the unit of the calculated county area?
#=====================================================================================
# Your answer here: square feet
#=====================================================================================

#=====================================================================================
# Q2: 
# Make a choropleth map showing the density of emission facilities (i.e., the count of emission points per square mile) by county. 
# Write your code chunk below. 
#=====================================================================================
# Your answer here:
counties.3435$de = counties.3435$TotEmisFac / (counties.3435$county_area * 3.587e-8)  
#this creates a new column of the converted sq foot to sq miles, with 3.587E-08 as the metric to convert the units 

# choropleth map
tm_shape(il) + tm_borders(lwd = 2) + tm_shape(counties.3435) + 
  tm_polygons(col = "de", style = "jenks", title = "Density of Emission Facilities") + 
  tm_layout(frame = FALSE, legend.outside = TRUE)
#=====================================================================================



########################
# CALCULATE FACILITY EMISSION AVERAGE BY COUNTY
########################

# Generate a point-in-polygon (PIP) dataset, joining county data to point data
pip <- st_join(emissions.3435, counties.3435, join = st_intersects)
head(pip)

# Using the PIP file, average all of emissions per county.
temp <- aggregate(pip$Emissions..Tons., by = list(pip$GEOID), mean)
head(temp)

# Rename the data in temp file
names(temp) <- c("GEOID", "EmissionAve")

# Join back to master county dataset
counties.3435 <- merge(counties.3435, temp, by = "GEOID", all.x = TRUE)
head(counties.3435)

# Map EmissionAve
tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties.3435) + tm_polygons(col = "EmissionAve", n=10,
                                    style = "jenks", title = "Emission Ave") +
  tm_layout(frame = FALSE, legend.outside = TRUE) 



########################
# BUFFER ANALYSIS: considering that emission often diffuses from the point source.
######################## 

# Generate 5-mile buffers. Note: 1 mile = 5280 ft (using EPSG 3435)
emission_buffers <- st_buffer(emissions.3435, 5280*5)
glimpse(emission_buffers) #Take a look at the transposed data: check https://dplyr.tidyverse.org/reference/glimpse.html

# Map the buffers
tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties) + tm_polygons(alpha = 0.5) +  
  tm_shape(emission_buffers) + tm_borders(col = "tomato1") +
  tm_shape(emissions.3435) + tm_dots(col = "black")



########################
# CALCULATE BUFFER COUNT BY COUNTY
########################

# First we identify how many times buffers overlap counties.
# Then we use lengths() to find out how many items are present in each county. 
counties.3435$bufferCt <- lengths(st_intersects(counties.3435, emission_buffers))
head(counties.3435)

tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties.3435) + tm_polygons("bufferCt", n=10,
                    style = "jenks", title = "CountBuffer") + 
  tm_layout(frame = FALSE, legend.outside = TRUE)


#======================================================================================
# Q3: 
# Compare the buffer count by county vs the point count by county. What difference do you notice? 
#=====================================================================================
# Your answer here: 
comparing = counties.3435[c("TotEmisFac", "bufferCt")]
comparing$difference = comparing$TotEmisFac - comparing$bufferCt
mean(comparing$difference)

#We can see that the average difference between the buffer count by county and point count by county is about -5.284314
#This suggests that on average, there is more buffer counts by county than point count by county, which may be due to 
#how we calculated the buffer count by county, through the number of polygons intersecting in counties.3435 with the emission buffers. 
#=====================================================================================


#======================================================================================
# Q4: Further quantify the effects of emission diffusion/dispersion
# Integrate what you have learned thus far to answer the following two questions. 
# There are several ways to solve each of the questions. 

# Q4.1. Calculate the number of counties affected by each of the emission buffer zones. 
# Write your code chunk below and map the result as a GRADUATED rSYMBOLOGY MAP. Be creative in the mapping design.

# Q4.2. Calculate the percentage of area in each county affected by emission diffusion. 
# Write your code chunk below and map the result as a choropleth MAP.
#=====================================================================================
# Your answers here:
#q4.1
counties.3435$bufferCt <- lengths(st_intersects(counties.3435, emission_buffers))
tm_shape(il) + tm_borders(lwd = 2) +
  tm_shape(counties.3435) + tm_polygons("bufferCt", alpha = 0.4,
              style = "jenks", title = "CountBuffer") + tm_layout(frame = FALSE, legend.outside = TRUE)
#unfinished, ran out of time.
#q4.2
#ran out of time unfortunately

#=====================================================================================