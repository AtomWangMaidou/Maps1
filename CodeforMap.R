library(tidyverse)
library(osmdata)
library(sf)
library(maptools)
library(raster)
library(RColorBrewer)
library(sp)
library(gstat)
library(osrm)
library(ggmap)
library(ggtext)
library(cartography)
library(showtext)
library(readxl)
library(fuzzyjoin)
library(chron)
library(kableExtra)
#raster overrides the dplyr select function, so I assigned it back
select <- dplyr::select

#三甲医院文件import&tidy
dat_1 <- read_csv("北京的三甲医院.csv")
head(dat_1)  %>%
  kable() %>%
  kable_styling(c("striped", "hover", "condensed"))

#Beijing shape file import, saved as a kable, named as 'BJ'
BJ <- st_read("北京市行政区划矢量文件/北京市.shp", quiet = TRUE)
BJ <- select(BJ, district, geometry) #'district' and 'geometry' columns selected
head(BJ)  %>%
  kable() %>%
  kable_styling(c("striped", "hover", "condensed"))

#Beijing shape file import, to-be-visualized, named as 'BJ_df'
BJ_df <- rgdal::readOGR(path.expand("北京市行政区划矢量文件"),layer=c("北京市"))

#Set the 'longitude' and 'latitude' columns as coordinates
coordinates(dat_1) <- ~ Longitude + Latitude
proj4string(dat_1) <- proj4string(BJ_df)

#using the over() function  to check in which of the 16 regions a hospital is located
dat_1_df <-  bind_cols(as_tibble(dat_1),
                              sp::over(dat_1, BJ_df, fun = "sum"))
dat_1_df

# summarize the number of hospitals per region
BJ <- BJ %>%
  left_join(
    dat_1_df %>%
      group_by(district) %>%
      summarise(N_hospitals = n()),
    by = "district")
#BJ[is.na(BJ)] <- 0
#BJ

# create colour breaks
bks <- getBreaks(v = BJ$N_hospitals, method = "jenks", nclass = 7)
cols <- carto.pal(pal1 = "orange.pal", n1 =7)

# plot the background
plot(BJ$geometry, border = NA, col = NA, bg = "#A6CAE0")

# plot a choropleth with number of hospitals per region
choroLayer(
  add = TRUE,
  x = BJ,
  var = "N_hospitals",
  breaks = bks,
  border = "burlywood3",
  col = cols,
  legend.pos = "topleft",
  legend.values.rnd = 1,
  legend.title.txt = "number of hospitals\nper district"
)

# plot the hospital points
plot(dat_1, lwd = 0.5, add = T)

# add labels
layoutLayer(title = "Locations of Grade A Tetiary Hospitals, Beijing",
            author = "Data by OSM and Maidou Wang · Visualization by Maidou Wang",
            frame = TRUE,
            scale = "auto",
            north = TRUE,
            theme = "orange.pal")
#######

longest_travel_time <- points_in_polygon_with_travel_times[which.max(points_in_polygon_with_travel_times$mintime), ]

longest_travel <- osrmTable(src = data.frame(id = 1 ,
                                             lon = longest_travel_time[1,4],
                                             lat = longest_travel_time[1,5]),
                            dst = hospital_locations_lat_long,
                            measure = "duration")


longest_travel_df <- tibble(id = 1,
                            longest_travel$sources[1],
                            longest_travel$sources[2],
                            mintime = apply(longest_travel$durations, 1, min),
                            lon_end = longest_travel$destinations[which.min(longest_travel$durations), 1],
                            lat_end = longest_travel$destinations[which.min(longest_travel$durations), 2])

route <- osrmRoute(src = longest_travel_df[,c(1,2,3)],
                   dst = longest_travel_df[,c(1,5,6)],
                   overview = "full",
                   returnclass = "sf")

LTT <- sub(":\\d{2}", "", times((route$duration%/%60 +  route$duration%%60/3600)/24))
LR <- route$distance

# Display the path
plot(BJ$geometry, border = "black", col = "white", bg = "grey")
plot(st_geometry(route), lwd = 1, add = TRUE, col = "red")


########

# for points with more than 2 hour driving distance, I cap them at 2 hours (for plotting purposes)

points_in_polygon_with_travel_times <- points_in_polygon_with_travel_times %>%
  mutate(mintime_capped = ifelse(mintime > 120, 120, mintime))


ggplot() +
  geom_polygon(data = BJ_df,
               aes(x=long, y=lat, group = group),
               fill = NA, col = "black") +
  geom_point(data = points_in_polygon_with_travel_times,
             aes(x = lon.org, y = lat.org, col = mintime_capped), size = .9, alpha = .9) +
  viridis::scale_color_viridis(option = "magma", direction = -1) +
  theme_minimal()+
  coord_map()
