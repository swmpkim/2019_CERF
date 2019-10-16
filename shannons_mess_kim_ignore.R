library(tidyverse)
library(here)
library(janitor)

# how will this data work?
df <- read.csv(here('data', '2016_waterquality.csv')) %>%
  janitor::clean_names()
glimpse(df)

# oh, great, I grabbed station information from elkhorn that reports LEVEL...sigh

# mess with the dates
df$datetime <- as.POSIXct(df$date_time_stamp, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')
# lubridate to pull out date information
# pull out month information to create monthly average
df$month <- month(df$datetime)

# will the calculations work?
df %>%
  select(-is_swmp, -f_record, -f_temp, -f_sp_cond, -f_sal, -f_do_pct, -f_do_mgl, -f_p_h, -f_turb,
         -f_depth, -f_c_depth, -f_level, -f_c_level, -f_chl_fluor, -x, -date_time_stamp,
         -day, -year, -datetime) %>%
  group_by(station_code, month) %>%
  summarise_all(list(mean), na.rm = TRUE) %>%
  ggplot() +
  geom_point(aes(x = temp, y = do_mgl, color = station_code), size = 4)

# create file
df_new <- df %>%
  select(-is_swmp, -f_record, -f_temp, -f_sp_cond, -f_sal, -f_do_pct, -f_do_mgl, -f_p_h, -f_turb,
         -f_depth, -f_c_depth, -f_level, -f_c_level, -f_chl_fluor, -x, -date_time_stamp,
         -day, -year, -datetime) %>%
  group_by(station_code, month) %>%
  summarise_all(list(mean), na.rm = TRUE)
write.csv(df_new, 'data/new_waterquality.csv')

# read in file and play around
df2 <- read.csv(here::here('data', 'new_waterquality.csv')) %>%
  janitor::clean_names()
glimpse(df2)

# ?? is there a relationship between dissolved oxygen and temperature?
ggplot(data = df2) +
  geom_point(mapping = aes(x = temp, y = do_mgl))
# looks like it!

ggplot(data = df2) +
  geom_point(mapping = aes(x = temp, y = do_mgl), size = 3) # change size just to see the points better

# let's look at the stations
ggplot(data = df2) +
  geom_point(aes(x = temp, y = do_mgl, colour = station_code), size = 3)

# looks like some stations may be clustered together, what else can we use to further investigate?
ggplot(data = df2) +
  geom_point(aes(x = temp, y = do_mgl, colour = coast), size = 3)

ggplot(data = df2) +
  geom_point(aes(x = temp, y = do_mgl, size = month), size = 3)

ggplot(data = df2) +
  geom_point(aes(x = temp, y = do_mgl, colour = coast), size = 3) +
  geom_smooth(aes(x = temp, y = do_mgl, color = coast))

# looks good!
