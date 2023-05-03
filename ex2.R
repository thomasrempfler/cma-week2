https://github.com/thomasrempfler/cma-week2.git

# ex2

# Date: 03.05.2023

# Author: Thomas Rempfler


library(dplyr) 
library(ggplot2)
library(readr) 
library(sf) 

# task 1 ----
wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)
  
# task 2 ----

# time intervals?
ws <- wildschwein_BE |> 
  group_by(TierID) |> # ...group it by TierID
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))
ws

# how many ids?
ggplot(ws, aes(DatetimeUTC, TierName)) +
  geom_point()

ggplot(ws, aes(timelag/60)) +
  geom_histogram(binwidth = 1) +
  lims(x = c(0, 5000/60)) +
  scale_y_log10()

# explore schedules
ggplot(ws, aes(DatetimeUTC, timelag, colour = TierName)) +
  geom_point()

# task 3
ws <- ws |> # Take wildschwein...
  group_by(TierID) |> # ...group it by TierID
  mutate(sl = sqrt((E - lead(E))^2 + (N - lead(N))^2))

ws <- ws |> 
  mutate(speed_ms = sl/timelag)
ws

ggplot(ws, aes(speed_ms)) +
  geom_histogram() +
  scale_y_log10()




ws |> # Take wildschwein...
  group_by(TierID) |> # ...group it by TierID
  summarise( # Summarise the data...
    mean_timelag = mean(timelag, na.rm = T) # ...by calculating the mean timelag
  )