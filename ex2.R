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

ws |> 
  group_by(TierID) |> 
  summarise(mean_timelag = mean(timelag, na.rm = T))
    
    
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


# task 4 ----
caro <- read_delim("caro60.csv", ",")

# every 3rd position
caro_3 <- caro |> 
  slice(seq(from = 1, to = nrow(caro), by = 3))

# every 6th position
caro_6 <- caro |> 
  slice(seq(from = 1, to = nrow(caro), by = 6))

# every 9th position
caro_9 <- caro |> 
  slice(seq(from = 1, to = nrow(caro), by = 9))

# timelage, step length, speed for every caro_x
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE) |> 
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC), units = "mins")) |> 
  mutate(sl = sqrt((E - lead(E))^2 + (N - lead(N))^2)) |> 
  mutate(speed_ms = sl/timelag)

caro_3 <- st_as_sf(caro_3, coords = c("E", "N"), crs = 2056, remove = FALSE) |> 
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC), units = "mins")) |> 
  mutate(sl = sqrt((E - lead(E))^2 + (N - lead(N))^2)) |> 
  mutate(speed_ms = sl/timelag)

caro_6 <- st_as_sf(caro_6, coords = c("E", "N"), crs = 2056, remove = FALSE) |> 
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC), units = "mins")) |> 
  mutate(sl = sqrt((E - lead(E))^2 + (N - lead(N))^2)) |> 
  mutate(speed_ms = sl/timelag)

caro_9 <- st_as_sf(caro_9, coords = c("E", "N"), crs = 2056, remove = FALSE) |> 
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC), units = "mins")) |> 
  mutate(sl = sqrt((E - lead(E))^2 + (N - lead(N))^2)) |> 
  mutate(speed_ms = sl/timelag)

# visualisations

caro_vis <- caro |> 
  bind_rows(caro_3, caro_6, caro_9) |> 
  filter(!is.na(timelag)) |> 
  mutate(timelag = as.factor(timelag)) 

# speed at different intervals (speed decreases with increasing timelag)
ggplot(caro_vis, aes(DatetimeUTC, speed_ms, colour = timelag)) +
  geom_line(linewidth = 1) + 
  ggtitle("Comparing derived speed at different sampling intervals") +
  theme_minimal() +
  xlab("Time") +
  ylab("Speed [m/s]")

# comparison of trajectories
caro_caro_3 <- caro |> 
  bind_rows(caro_3) |> 
  filter(!is.na(timelag)) |> 
  mutate(timelag = as.factor(timelag))

ggplot(caro_caro_3, aes(E, N, colour = timelag)) +
  geom_point() + 
  geom_path() +
  coord_cartesian() +
  ggtitle("Comparing original- with 3 minutes-resampled data") +
  theme_minimal() 

caro_caro_6 <- caro |> 
  bind_rows(caro_6) |> 
  filter(!is.na(timelag)) |> 
  mutate(timelag = as.factor(timelag))

ggplot(caro_caro_6, aes(E, N, colour = timelag)) +
  geom_point() + 
  geom_path() +
  coord_cartesian() +
  ggtitle("Comparing original- with 6 minutes-resampled data") +
  theme_minimal() 

caro_caro_9 <- caro |> 
  bind_rows(caro_9) |> 
  filter(!is.na(timelag)) |> 
  mutate(timelag = as.factor(timelag))

ggplot(caro_caro_9, aes(E, N, colour = timelag)) +
  geom_point() + 
  geom_path() +
  coord_cartesian() +
  ggtitle("Comparing original- with 9 minutes-resampled data") +
  theme_minimal() 
