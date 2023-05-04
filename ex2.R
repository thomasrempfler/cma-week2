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
  group_by(TierID) |> 
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))
ws

# how many individuals were tracked? -> 3 individuals (Sabi, Ruth, Rosa)
ggplot(ws, aes(DatetimeUTC, TierName)) +
  geom_point()

ws |>
  distinct(TierID)

# for how long were they tracked?
ws |> 
  group_by(TierName) |> 
  mutate(date_min = min(DatetimeUTC), date_max = max(DatetimeUTC)) |> 
  distinct(date_min, date_max)

# are there gaps?
ggplot(ws, aes(timelag/60)) +
  geom_histogram(binwidth = 1) +
  lims(x = c(0, 5000/60)) +
  scale_y_log10()

# were they tracked concurrently or sequentially?
ggplot(ws, aes(DatetimeUTC, timelag, colour = TierName)) +
  geom_point()

# what is the temporal sampling interval?
ws |> 
  group_by(TierName, timelag) |> 
  distinct()
  
ggplot(ws, aes(DatetimeUTC, timelag/60, colour = TierName)) +
  geom_point()


# task 3 ----

# euclidean distances
ws <- ws |> 
  group_by(TierID) |> 
  mutate(sl = sqrt((E - lead(E))^2 + (N - lead(N))^2))

# speed (m/s)
ws <- ws |> 
  mutate(speed_ms = sl/timelag)

ggplot(ws, aes(speed_ms)) +
  geom_histogram() +
  scale_y_log10()




ws |> # Take wildschwein...
  group_by(TierID) |> # ...group it by TierID
  summarise( # Summarise the data...
    mean_timelag = mean(timelag, na.rm = T) # ...by calculating the mean timelag
  )