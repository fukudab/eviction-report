# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

# Load in your data
evictions <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = FALSE)

# Compute some values of interest and store them in variables for the report
num_features <- ncol(evictions)

# How many evictions were there?
num_evictions <- nrow(evictions)

# Create a table (data frame) of evictions by zip code (sort descending)
by_zip <- evictions %>%
  group_by(Eviction.Notice.Source.Zipcode) %>%
  count() %>% # Counts many rows in a grouping (this time, zip code type) in data set
  arrange(-n) # The count() values are stored in new variable "n", so putting (-) arranges it in descending order

# Create a plot of the number of evictions each month in the dataset
as.Date("10/6/17", format = "%m/%d/%y") # Example of changing format of text

by_month <- evictions %>% # New dataframe from existing set "evictions"
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>% # Add column "date", changing format for all data in "File.Date" column
  mutate(month = floor_date(date, unit="month")) %>% 
  group_by(month) %>%
  count()
# Add column "month", lubridate's function called "floor_date" groups dates into different categories (this time, by month, and makes any date in that month into that group
# (ex: 11/21 will be pushed back into 11/01 with the rest of the November days)
# count how many rows are in each group of months


# Store plot in a variable
month_plot <- ggplot(data = by_month) +
  geom_line(mapping = aes(x = month, y = n), color = "red", alpha = 0.5) +
  labs(x = "Date", y = "Number of Evictions", title = "Evictions over time in San Francisco")

# Map evictions in 2017 


# Format the lat/long variables, filter to 2017
evictions_2017 <- evictions %>% 
  mutate(date = as.Date(File.Date, format="%m/%d/%y")) %>%  #changes date format
  filter(format(date, "%Y") == "2017") %>% # Takes data with year being 2017
  separate(Location, c("lat", "long"), ", ") %>% # splits column "Location" at comma into "lat" and "long" (since both coordinates are at one column, we want to separate them into their own column)
  mutate( # Make new columns official for latitude and longitude
    lat = as.numeric(gsub("\\(", "", lat)), # removes starting parentheses
    long = as.numeric(gsub("\\)", "", long)) # removes closing parentheses
  )


# Create a maptile background
base_plot <- qmplot(
  data = evictions_2017,        # name of the data frame
  x = long,                     # data feature for longitude
  y = lat,                      # data feature for latitude
  geom = "blank",               # don't display data points (yet)
  maptype = "toner-background", # map tiles to query
  darken = .7,                  # darken the map tiles
  legend = "topleft"            # location of legend on page
)


# Add a layer of points on top of the map tiles
evictions_plot <- base_plot +
  geom_point(mapping = aes(x = long, y = lat), color = "red", alpha = .3) +
  labs(title = "Evictions in San Francisco, 2017") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm")) # adjust spacing around the map


### When making Rmd file, make sure it's called "index.Rmd", so Github can automatically convert it
