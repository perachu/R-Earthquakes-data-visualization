# R-Earthquakes-data-visualization
### Using ggplot2 in R to visualize data for multi layers 

1) Import and clean our main data(Earthquakes in Southeast asia)
2) Import World map data and prepare country label(centroid calculation)
3) Plotting in layers
   - 1st layer: World map
   - 2nd layer: Earthquakes record
   - 3nd layer: Country label
   - setting datail of plot
  
### Import and clean our main data(Earthquakes in Southeast asia)
```r
# import library
library(dplyr)
library(tidyverse)

earthq <- read.csv("earthq_sea_2020to2025.csv", stringsAsFactors = FALSE)

# clean dataset
earthq_clean <- earthq %>% mutate(mag_range = case_when(
                              mag <= 4.9 ~ "< 5",
                              mag >= 5.0 & mag <= 5.9 ~ "5.0 - 5.9",
                              mag >= 6.0 & mag <= 6.9 ~ "6.0 - 6.9",
                              mag >= 7.0 & mag <= 7.9 ~ "7.0 - 7.9",
                              mag >= 8.0 ~ "≥ 8.0")) %>%
                           select(time, longitude, latitude, depth, mag, mag_range) %>%
                           arrange(mag)

glimpse(earthq_clean)
```

### Import World map data and prepare country label
```r
## import world map data
#install.packages("maps")         # For basic world map
#install.packages("mapdata")      # Extra detail

library(maps)
library(mapdata)

world_map <- map_data("world")
glimpse(world_map)

## cal centroid of country
seasia_map <- world_map %>%
              filter(long >= 90, long <= 130, lat >= -15, lat <= 25)

country_labels <- seasia_map %>%
                  group_by(region) %>%
                  summarize(
                  long = median(range(long)),
                  lat = median(range(lat))
                  )

glimpse(country_labels)
```

### Plotting in layers
```r
# SE Asia map plot

# 1st layer
ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group), 
               fill = "white",color = "#a8a8a8") +
               scale_fill_gradient() +
# 2nd layer
  geom_point(data = earthq_clean, 
             aes(x = longitude, y = latitude, size = mag_range, color = mag, alpha = mag)) +
             scale_color_gradient() + 
             guides(alpha = "none", size = guide_legend(reverse = TRUE)) +
# 3rd layer
  geom_text(data = country_labels,
            aes(x = long, y = lat, label = region),
            size = 6, color = "red") +
  
  coord_cartesian(xlim = c(88.59, 140), ylim = c(-12.37, 31.29)) +  # Zoom to SE Asia
  theme_minimal() +
  labs(title = "Earthquakes in Southeast Asia Region",
       subtitle = "Record duration: 2020 to 2025",
       caption = "Raw Data Source: USGS",
       color = "Magnitude", size = "Magnitude Range") +
  theme(
    plot.title = element_text(size = 24),       # Title size
    plot.subtitle = element_text(size = 18),    # Subtitle size
    plot.caption = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14),
  )
```

