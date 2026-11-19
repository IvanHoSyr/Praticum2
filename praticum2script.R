library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)

#Q1
counties_sf <- st_read("cb_2024_us_county_500k.shp")

ny_counties<- counties_sf%>% filter(STUSPS== "NY")

#Q2
largest_NY_county<- ny_counties %>%
  arrange(desc(ALAND))
head(largest_NY_county)

smallest_NY_county<- ny_counties %>%
  arrange(ALAND)
head(smallest_NY_county)

#Q3
ny_highpoints<- read.csv("nyshighpoints.csv")

#Q4,Q5,Q6
highest_NY_point<- ny_highpoints %>%
  arrange(desc(Elevation_Feet))
head(highest_NY_point)

lowest_NY_point<- ny_highpoints %>%
  arrange(Elevation_Feet)
head(lowest_NY_point)

library(lattice)
range(highest_NY_point$Elevation_Feet)
summary(highest_NY_point$Elevation_Feet)

ny_highpoints_edit <- ny_highpoints%>%
  mutate(
    elev_category = factor(
      case_when(
        Elevation_Feet == 220                          ~ "lowest",
        Elevation_Feet > 220  & Elevation_Feet <= 1590 ~ "low",
        Elevation_Feet > 1590 & Elevation_Feet < 2110  ~ "below median",
        Elevation_Feet == 2110                         ~ "median",
        Elevation_Feet > 2110 & Elevation_Feet <= 2594 ~ "above median",
        Elevation_Feet > 2594 & Elevation_Feet < 5344  ~ "high",
        Elevation_Feet == 5344                         ~ "highest"
      ),
      levels = c("lowest", "low", "below median", "median", "above median", "high", "highest")
    )
  )

ny_points_sf <- st_as_sf(
  ny_highpoints_edit,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)


color_cat <- c(
  "lowest"       = "#1a9850", # Deep lowland green
  "low"          = "#91cf60", # Light green
  "below median" = "#fee08b", # Pale yellow
  "median"       = "#fdae61", # Golden yellow-tan
  "above median" = "#b35806", # Light mountain brown
  "high"         = "#7f3b08", # Dark mountain brown
  "highest"      = "#d7191c"  # Peak crimson red
)

shape_cat <- c(
  "lowest"       = 8,  
  "low"          = 16,
  "below median" = 16,
  "median"       = 16,
  "above median" = 16,
  "high"         = 16,
  "highest"      = 8  
)


size_cat <- c(
  "lowest"       = 4.5,
  "low"          = 2.5,
  "below median" = 2.5,
  "median"       = 2.5,
  "above median" = 2.5,
  "high"         = 2.5,
  "highest"      = 4.5
)

highest_pt <- ny_points_sf %>%
  slice_max(Elevation_Feet, n = 1)
lowest_pt <- ny_points_sf %>%
  slice_min(Elevation_Feet, n = 1)
extreme_pts <- bind_rows(highest_pt, lowest_pt)

ggplot() +
  geom_sf(data = ny_counties, fill = "lightgrey", color = "grey55", linewidth = 0.25) +
  geom_sf(
    data = ny_points_sf,
    aes(color = elev_category, shape = elev_category, size = elev_category)
  ) +
  geom_sf_text(
    data = extreme_pts,  
    aes(label = High_Point_Name),
    size = 3.5,
    fontface = "bold",
    nudge_x = 0.25,
    nudge_y = 0.12
  ) +
  scale_color_manual(name = "Elevation Category", values = color_cat) +
  scale_shape_manual(name = "Elevation Category", values = shape_cat) +
  scale_size_manual(name = "Elevation Category", values = size_cat) +
  theme_map()

#Q7
library(tidycensus)
library(stringr)
library(scales)

#population
ny_counties_pop <- get_acs(
  geography = "county",
  variables = "B01003_001",
  state = "NY",
  year = 2024)

NY_county_merged <- left_join(ny_counties, ny_counties_pop, by = "GEOID")

NY_county_merged<- NY_county_merged%>% mutate(density=estimate/ALAND)

highest_pop <- NY_county_merged %>%
  slice_max(estimate, n = 1)
lowest_pop <- NY_county_merged %>%
  slice_min(estimate, n = 1)
extreme_pop <- bind_rows(highest_pop, lowest_pop)
# The county with the largest population is King County. The county with the smallest population is Hamilton. County.

ny_pop_color <- NY_county_merged %>%
  ggplot() +
  geom_sf(aes(fill = estimate), linewidth = 0.2, color = "gray80") +
  scale_fill_distiller(
    palette = "Purples",
    direction = 1,              
    trans = "log10",
    labels = label_comma()
  ) +
  geom_sf_text(
    data = extreme_pop,
    aes(label = NAME.x),
    size = 3.5,
    fontface = "bold",
    nudge_x = -0.30,
    nudge_y = 0.12
  ) +
  labs(fill = "Population") +  
  theme_map()

ny_pop_color

#Q9
#density
highest_den <- NY_county_merged %>%
  slice_max(density, n = 1)
lowest_den <- NY_county_merged %>%
  slice_min(density, n = 1)
extreme_den <- bind_rows(highest_den, lowest_den)
# The most densed county in New York State is New York County. The least densed county in New York State is Hamilton County

ny_den_color<-NY_county_merged %>%
  ggplot()+
  geom_sf(aes(fill = density),linewidth = 0.2, color = "gray80") +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,              
    trans = "log10",
    labels = label_comma()
  ) +
  geom_sf_text(
    data = extreme_den,
    aes(label = NAME.x),
    size = 3.5,
    fontface = "bold",
    nudge_x = -0.30,
    nudge_y = 0.12
  ) +
  labs(fill = "Density") +  
  theme_map()
ny_den_color

#size of each county
largest_area <- NY_county_merged %>%
  slice_max(ALAND, n = 1)
smallest_area <- NY_county_merged %>%
  slice_min(ALAND, n = 1)
extreme_area <- bind_rows(largest_area, smallest_area)

#St.Lawrence is the largest county in New York State. New York County is the smallest county in New York State.

ny_counties_size<-NY_county_merged %>%
  ggplot()+
  geom_sf(aes(fill = ALAND),linewidth = 0.2, color = "gray80") +
  scale_fill_distiller(
    palette = "Oranges",
    direction = 1,              
    trans = "log10",
    labels = label_comma()
  ) +
  geom_sf_text(
    data = extreme_area,
    aes(label = NAME.x),
    size = 3.5,
    fontface = "bold",
    nudge_x = -0.30,
    nudge_y = 0.12
  ) +
  labs(fill = "Size") +  
  theme_map()
ny_counties_size
