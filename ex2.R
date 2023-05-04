https://github.com/thomasrempfler/cma-week2.git

# ex2

# date: 03.05.2023

# update: 04.05.2023

# author: Thomas Rempfler


library(dplyr) 
library(ggplot2)
library(readr) 
library(sf) 
library(zoo)
library(forcats)
library(mapview)

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


# task 5 ----

# # examples
# example <- rnorm(10)
# rollmean(example, k = 3, fill = NA, align = "left")
# rollmean(example, k = 4, fill = NA, align = "left")


# comparison of different window sizes

# 5 (mins)
caro_5m <- caro |> 
  mutate(window = "5 mins") |> 
  mutate(mean_speed = rollmean(speed_ms, k = 5, fill = NA, align = "left"))
  
# 15 (mins)
caro_15m <- caro |> 
  mutate(window = "15 mins") |> 
  mutate(mean_speed = rollmean(speed_ms, k = 15, fill = NA, align = "left"))

# 30 (mins)
caro_30m <- caro |> 
  mutate(window = "30 mins") |> 
  mutate(mean_speed = rollmean(speed_ms, k = 30, fill = NA, align = "left"))

# 60 (mins)
caro_60m <- caro |> 
  mutate(window = "60 mins") |> 
  mutate(mean_speed = rollmean(speed_ms, k = 60, fill = NA, align = "left"))

# visualisations mean speed
caro_mean_speed <- caro_5m |> 
  bind_rows(caro_15m, caro_30m, caro_60m) |> 
  filter(!is.na(mean_speed)) |> 
  mutate(window = as.factor(window)) |> 
  mutate(window = fct_relevel(window, c("5 mins", "15 mins", "30 mins", "60 mins")))

# speed at different intervals (speed decreases with increasing timelag)
ggplot(caro_mean_speed, aes(DatetimeUTC, mean_speed, colour = window)) +
  geom_line(linewidth = 1) + 
  ggtitle("Comparing derived mean speed at different moving windows") +
  theme_minimal() +
  xlab("Time") +
  ylab("Mean speed [m/s]")


# task 6 ----
rd <- read_delim("data/rd_rid_zhaw.csv", ",")
summary(rd)

rd_sf <- st_as_sf(rd, coords = c("x", "y"), crs = 2056, remove = FALSE)

# time intervals?
rd_sf <- rd_sf |> 
  group_by(Animal_no) |> 
  mutate(timelag = as.numeric(difftime(lead(acquisition_time), acquisition_time), units = "secs")) |> 
  mutate(sl = sqrt((x - lead(x))^2 + (y - lead(y))^2)) |> 
  mutate(speed_ms = sl/timelag) |> 
  mutate(Animal_no = as.factor(Animal_no))
table(rd_sf$timelag)

# how many individuals were tracked? -> 2 individuals (Nr. 38, 53)
ggplot(rd_sf, aes(acquisition_time, Animal_no)) +
  geom_point()

rd_sf |>
  distinct(Animal_no)

# for how long were they tracked?
rd_sf |> 
  group_by(Animal_no) |> 
  mutate(date_min = min(acquisition_time), date_max = max(acquisition_time)) |> 
  distinct(date_min, date_max)

# are there gaps?
ggplot(rd_sf, aes(timelag)) +
  geom_histogram(binwidth = 1) +
  lims(x = c(0, 15000)) 

# were they tracked concurrently or sequentially?
ggplot(rd_sf, aes(acquisition_time, timelag/3600, colour = Animal_no)) +
  geom_point()

# what is the temporal sampling interval?
rd_sf |> 
  group_by(Animal_no, timelag/60) |> 
  distinct() |> count()

rd_sf |> 
  group_by(Animal_no) |> 
  summarise(mean_timelag = mean(timelag, na.rm = T)/60)

# speed of both individuals?
ggplot(rd_sf, aes(acquisition_time, speed_ms, colour = Animal_no)) +
  geom_line(linewidth = 1) + 
  ggtitle("Comparing derived speed for 2 individuals") +
  theme_minimal() +
  xlab("Time") +
  ylab("Speed [m/s]") +
  facet_wrap(~Animal_no)

# mapping
ggplot(rd_sf, aes(x, y, colour = Animal_no)) +
  geom_point() + 
  geom_path() +
  coord_fixed() +
  ggtitle("Comparing 2 individuals") +
  theme_minimal() +
  facet_wrap(~Animal_no)

mapview(rd_sf, zcol = "Animal_no", alpha.regions = .5)
