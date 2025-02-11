library(sf)
library(terra)
library(dplyr)
library(spData)
library(readxl)
library(readr)
census_22 = read_xls("C:/Users/lyons/OneDrive/Desktop/ggis224/est22us.xls")
water_data = read_xlsx("C:/Users/lyons/OneDrive/Desktop/ggis224/narrowresult.xlsx")
data = read.csv("C:/Users/lyons/Downloads/ACSST1Y2022.S1701-2024-05-06T225551.csv")
clean_convert <- function(column) {
  as.numeric(gsub("[%,]", "", column))
}
data <- data |>
  mutate(across(contains("Estimate"), ~clean_convert(.))) |>
  mutate(across(contains("Margin of Error"), ~clean_convert(.)))
colnames(data) <- gsub("Illinois!!", "", colnames(data))
colnames(data) <- gsub("!!", "_", colnames(data))
data <- data %>%
  replace(is.na(.), 0)
print(head(data))
w = read_csv("C:/Users/lyons/Downloads/narrowresult.csv")
table(water_data$ResultMeasure/MeasureUnitCode)

