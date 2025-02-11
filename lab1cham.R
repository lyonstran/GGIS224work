library(tmap)
library(sf)
Cha_tracts = st_read("C:/Users/lyons/OneDrive/Desktop/ggis224/Lab1_data/Champaign_county_census_tracts.shp")
Cha_zips = st_read("C:/Users/lyons/OneDrive/Desktop/ggis224/Lab1_data/Champaign_county_zip_codes.shp")
Cha_tracts.3435 <- st_transform(Cha_tracts, "EPSG:3435")


tm_shape(Cha_tracts.3435) + tm_fill(col = "gray80") + tm_borders(alpha = 1, col = "black") + 
  
  tm_layout(main.title = "Census tracts and zip code boundaries Champaign County, IL", main.title.size = 0.80) + 
  
  tm_scale_bar(position = ("left"), lwd = 0.3) + tm_shape(Cha_zips) + tm_borders(lwd = 0.1, col = "#e4007c") +
  
  tm_text("zip", size = 0.5)
