library(sf)
library(terra)
library(dplyr)
library(spData)
data(us_states)
data(us_states_df)

# Q1. Create a new object called us_states_name that contains only the NAME column from
# the us_states object using either base R ([) or tidyverse (select()) syntax.
# What is the class of the new object and what makes it geographic?

us_states_name = select(us_states, NAME)
class(us_states_name)
#The class of the new object us_states_name is an sf data.frame. It's geometry attribute makes it geometric. 

#____________________________________________________________________________________________________________

# Q2. Select columns from the us_states object which contain population data. 
# Obtain the same result using three different ways. Hint: try to use helper functions, 
# such as  -> contains or  -> matches from dplyr.

method1 = select(us_states, total_pop_10, total_pop_15) #uses select with default arguments
method2 = us_states |> select(contains("total_pop_")) #uses contains() 
method3 = us_states |> select(matches("total_pop_")) #uses matches()

#____________________________________________________________________________________________________________


# Q3. Find and list all states with each of the following characteristics: 
#   A. Belong to the Midwest region.
question3A = us_states |> filter(REGION == "Midwest")
  
#   B. Belong to the West region, had an area below 250,000 km2 and in 2015 a population greater than 
#   5,000,000 residents. (hint: you may need to use the function units::set_units() or as.numeric()).
question3B = us_states |> filter(REGION == "West", AREA < units::set_units(250000, km^2) , total_pop_15 > 5000000) 

#   C. Belong to the South region, had an area larger than 150,000 km2 and a total population in 2015 
#   larger than 7,000,000 residents.
question3C = us_states |> filter(REGION == "South", AREA > units::set_units(150000, km^2), total_pop_15 > 7000000)

#____________________________________________________________________________________________________________

# Q4. What was the total population in 2015 in the us_states dataset? 
# What was the minimum and maximum total population in 2015?
us_states |> summarise(total_population = sum(total_pop_15), smallest_population = min(total_pop_15), 
            largest_population = max(total_pop_15))
#The total population was 314375347 people, smallest population was 579679 people, and 
#largest population was 38421464 people in the US.

#____________________________________________________________________________________________________________

# #Q5. How many states are there in each region?
us_states |>
  group_by(REGION) |>
  summarise(number_of_states = n())
#There are 9 states in the Northeast, 12 states in the Midwest, 17 states in the South, and 11 states in the West.

#____________________________________________________________________________________________________________

# Q6. What was the minimum and maximum total population in 2015 in each region? 
#What was the total population in 2015 in each region?
us_states |> group_by(REGION) |> summarise(minimum_pop = min(total_pop_15), maximum_pop = max(total_pop_15), 
  total_pop = sum(total_pop_15))

#Northeast - max: 19673174, min: 626604, total = 55989520
#MidWest - max: 12873761, min: 721640, total = 67546398
#South - max: 26538614, min: 647484, total = 118575377
#Northeast - max: 579679, min: 38421464, total = 72264052 

#____________________________________________________________________________________________________________

# Q7. Add variables from us_states_df to us_states, and create a new object called us_states_stats. 
# What function did you use and why? Which variable is the key in both datasets? 
# What is the class of the new object?

us_states_stats = us_states |> left_join(us_states_df, join_by("NAME" == "state"))
class(us_states_stats) 

#For this problem, I used the left_join function after the pipe operator in order to merge the us_states_df
#to us_states as this was the function they demonstrated in Chapter 3. The variable that is the key to merge these
#two was "NAME" for us_states and "state" for us_states_df. The class of the new object is sf data.frame. 

#____________________________________________________________________________________________________________

# Q8. us_states_df has two more rows than us_states. 
# How can you find them? (hint: try to use the dplyr::anti_join() function)

setdiff(us_states_df$state, us_states$NAME)
#The two rows that us_states_df has over us_states is Alaska and Hawaii

#____________________________________________________________________________________________________________

# Q9. Calculate the change in the number of residents living below 
# the poverty level between 2010 and 2015 for each state. Write the calculated result in a new 
# variable called poverty_change and map the result
# (you don't need to submit the map but your script must be able to generate the map).

us_states_stats2 = us_states_stats |> mutate(poverty_change = poverty_level_15 - poverty_level_10)
plot(us_states_stats2["poverty_change"])

#______________________________________________________________________________________________

# Q10. What is the region with the largest increase in people living below the poverty line?

us_states_stats2 |> group_by(REGION) |> summarise(poverty_change_for_region = sum(poverty_change)) |>
  filter(poverty_change_for_region == max(poverty_change_for_region))
#The Southern region in the US had the largest increase in people living below the poverty line. 







