############################################################
# Ecosystem service estimation using remote sensing results
# 09th. October 2019 First launch
# **th. ****** **** Correction
#
# Entire codes will be synchronized on github.
# https://github.com/yuzuruu/remote_sensing_based_ecosystem_service


# ---- load.library ----
library(rgdal)
library(spatstat)
library(ggspatial)
library(ggsn)
library(GGally)
library(tidyverse)
library(viridis)
library(viridisLite)

# ---- read.data ----
# To transform datum, please refer to the following page.
# https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r
# read data
hh.2010 <- readxl::read_excel("../Household_survey_data/2010_household_survey_for_R.xlsx")
# Transform the UTM into lonlat
utm.xy <- 
  hh.2010 %>% 
  dplyr::select(X, Y) %>% 
  SpatialPoints(., proj4string=CRS("+proj=utm +zone=48 +datum=WGS84")) %>% 
  spTransform(., CRS("+proj=longlat +datum=WGS84")) %>% 
  as_tibble()
colnames(utm.xy) <- c("lon","lat")
# Add the transformed position to the original data
hh.2010 <- bind_cols(hh.2010, utm.xy)
# select required values
## choose any of requisite values
hh.2010.sub <- 
  hh.2010 %>% 
  dplyr::select(lon,
                lat,
                Code,
                age,
                sex,
                edu_le,
                village,
                disct,
                prov,
                total_area,
                total_inco,
                total_invest
  ) %>% 
  # Omit observations containing NAs
  na.omit() %>% 
  #Transform the values into factor
  dplyr::mutate(sex = factor(sex, levels = c(1,2)),
                edu_le = factor(edu_le, levels = c(1,2,3,5,6))
  )
#
# END ---

# ---- summary.statistics.and.histogram ----
# Make a descriptive statistics
summary.hh.2010.sub <- 
  hh.2010.sub %>% 
  group_by(disct, village) %>% 
  summarise(n = n(),
            min.total.area = min(total_area),
            mean.total.area = mean(total_area),
            median.total.area = median(total_area),
            max.total.area = max(total_area),
            sd.total.area = sd(total_area)
  )
# print to confirm summary table content
print(summary.hh.2010.sub, n = Inf)
# save the table
write.csv(summary.hh.2010.sub, "summary.hh.2010.sub.csv")

# wrapped histogram by district
hh.2010.sub %>% 
  ggplot(aes(x = total_area))+
  geom_histogram()+
  facet_wrap(~ disct) +
  theme_classic()
# save the histogram
ggsave("hh.2010.sub.hist.pdf")

# density plot by district
hh.2010.sub %>% 
  ggplot(aes(x = total_area, colour = disct))+
  geom_line(stat = "density", size = 1)+
  scale_color_viridis_d(option = "viridis", aesthetics = "colour") +
  labs(x = "Total area", y = "Density", colour = "District", ) +
  theme_classic()
# save the density plot
ggsave("hh.2010.sub.density.pdf")
#
# END ---

# ---- map.data ----
# To download sf file, please refer to the following website.
# The sf file is much lighter than .shp and suitable for R.
# https://gadm.org/download_country_v3.html
#
# read the sf file
vnm.adm.01 <- readRDS("gadm36_VNM_1_sf.rds") # province
vnm.adm.02 <- readRDS("gadm36_VNM_2_sf.rds") # district
vnm.adm.03 <- readRDS("gadm36_VNM_3_sf.rds") # commune
vnm.adm <- vnm.adm.03
# Pick up data of Ca Mau province
# Somehow it does not work.
vnm.adm %>% dplyr::filter(NAME_1 == "Ca Mau")
# It works.
vnm.adm.cm <- 
  vnm.adm %>% 
  dplyr::filter(GID_1 == "VNM.13_1")
# To overwrite boundaries of province
vnm.adm.cm.02 <- 
  vnm.adm.02 %>% 
  dplyr::filter(GID_1 == "VNM.13_1")

# Draw a map of administrative boundaries and overwrap survey points
# We use OpenStreet map for base layer.
# We can use google map. It, however, requires API when we use that many times.
# Referring to the following website, we are able to switch 
# type of map.
# https://wiki.openstreetmap.org/wiki/Tile_servers
# To overwrite the boundaries of districts and ones of wards, please refer to the 
# following website.
# https://stackoverflow.com/questions/49497182/ggplot-create-a-border-overlay-on-top-of-map

# ---- map.total.area ----
# colouring by total area
# total area
map.osm.vnm.cm.total.area <- 
ggplot(vnm.adm.cm) + 
  annotation_map_tile(zoomin = 0, # This argument is adjusted automatically even if we fix it. 
                      type = "https://a.tile.openstreetmap.org/${z}/${x}/${y}.png"
  ) + 
  # draw boundaries of ward
  # adjust administrative boundaries' color, density, and lines' width.
  geom_sf(alpha = 0.5, 
          colour = "grey60", 
          size = 0.5
          ) +
  # overwrite boundaries of district
  geom_sf(fill = "transparent", 
          color = "gray60", 
          size = 1, 
          data = . %>% 
            group_by(GID_2) %>% 
            summarise()
          ) +
  # Adjust area
  xlim(104.6, 105.5) +
  ylim(8.5, 9.0) +
  # Overwrap survey point using survey results
  geom_point(
    data = hh.2010.sub,
    aes(x = lon, y = lat, fill = total_area),
    colour = "grey28",
    shape = 21,
    size = 2.5
  ) +
  scale_fill_viridis_c(option = "viridis",
                       begin = 1,
                       end = 0
                       ) +
  labs(x = "Longitude", 
       y = "Latitude",
       caption = "\U00a9 OpenStreetMap contributors"
       ) +
  theme_minimal() + 
  # adjust scalebar's preferences
  ggsn::scalebar(x.min = 105.0,
                 x.max = 105.4,
                 y.min = 8.55,
                 y.max = 8.60, 
                 dist_unit = "km",
                 dist = 20, 
                 st.size = 4,
                 st.dist = 0.25,
                 height = 0.25,
                 model = "WGS84", 
                 transform = TRUE,
                 location = "bottomright",
                 box.fill = c("grey30", "white"), # left and right
                 box.color = "white",
                 st.color = "white"
  ) 
# save the map
ggsave("map.osm.vnm.cm.total.area.pdf" ,
       plot = map.osm.vnm.cm.total.area
       )

# ---- map.total.income ----
# colouring by total income
map.osm.vnm.cm.total.inco <- 
  ggplot(vnm.adm.cm) + 
  annotation_map_tile(zoomin = 0, # This argument is adjusted automatically even if we fix it. 
                      type = "https://a.tile.openstreetmap.org/${z}/${x}/${y}.png"
  ) + 
  # adjust administrative boundaries' color, density, and lines' width.
  geom_sf(alpha = 0.5, 
          colour = "grey60", 
          size = 0.5
  ) +
  # overwrite boundaries of district
  geom_sf(fill = "transparent", 
          color = "gray60", 
          size = 1, 
          data = . %>% 
            group_by(GID_2) %>% 
            summarise()
  ) +
  # Adjust area
  xlim(104.6, 105.5) +
  ylim(8.5, 9.0) +
  # Overwrap survey point using survey results
  geom_point(
    data = hh.2010.sub,
    aes(x = lon, y = lat, fill = total_inco),
    colour = "grey28",
    shape = 21,
    size = 2.5
  ) +
  scale_fill_viridis_c(option = "viridis",
                       begin = 1,
                       end = 0
  ) +
  labs(x = "Longitude", 
       y = "Latitude",
       caption = "\U00a9 OpenStreetMap contributors"
  ) +
  theme_minimal() + 
  # adjust scalebar's preferences
  ggsn::scalebar(x.min = 105.0,
                 x.max = 105.4,
                 y.min = 8.55,
                 y.max = 8.60, 
                 dist_unit = "km",
                 dist = 20, 
                 st.size = 4,
                 st.dist = 0.25,
                 height = 0.25,
                 model = "WGS84", 
                 transform = TRUE,
                 location = "bottomright",
                 box.fill = c("grey30", "white"), # left and right
                 box.color = "white",
                 st.color = "white"
  ) 
# save the map
ggsave("map.osm.vnm.cm.total.inco.pdf" ,
       plot = map.osm.vnm.cm.total.inco
)
#
# END ---

# ---- map.total.investment ----
# colouring by total investment
map.osm.vnm.cm.total.invest <- 
  ggplot(vnm.adm.cm) + 
  annotation_map_tile(zoomin = 0, # This argument is adjusted automatically even if we fix it. 
                      type = "https://a.tile.openstreetmap.org/${z}/${x}/${y}.png"
  ) + 
  # adjust administrative boundaries' color, density, and lines' width.
  geom_sf(alpha = 0.5, 
          colour = "grey60", 
          size = 0.5
  ) +
  # overwrite boundaries of district
  geom_sf(fill = "transparent", 
          color = "gray60", 
          size = 1, 
          data = . %>% 
            group_by(GID_2) %>% 
            summarise()
  ) +
  # Adjust area
  xlim(104.6, 105.5) +
  ylim(8.5, 9.0) +
  # Overwrap survey point using survey results
  geom_point(
    data = hh.2010.sub,
    aes(x = lon, y = lat, fill = total_invest),
    colour = "grey28",
    shape = 21,
    size = 2.5
  ) +
  scale_fill_viridis_c(option = "viridis",
                       begin = 1,
                       end = 0
  ) +
  labs(x = "Longitude", 
       y = "Latitude",
       caption = "\U00a9 OpenStreetMap contributors"
  ) +
  theme_minimal() + 
  # adjust scalebar's preferences
  ggsn::scalebar(x.min = 105.0,
                 x.max = 105.4,
                 y.min = 8.55,
                 y.max = 8.60, 
                 dist_unit = "km",
                 dist = 20, 
                 st.size = 4,
                 st.dist = 0.25,
                 height = 0.25,
                 model = "WGS84", 
                 transform = TRUE,
                 location = "bottomright",
                 box.fill = c("grey30", "white"), # left and right
                 box.color = "white",
                 st.color = "white"
  ) 
# save the map
ggsave("map.osm.vnm.cm.total.invest.pdf" ,
       plot = map.osm.vnm.cm.total.invest
)

#
# END ---

# ---- pair plot ----
pairs.by.district <- 
  hh.2010.sub %>%
  dplyr::select(disct, total_area, total_inco, total_invest) %>% 
  ggpairs(aes_string(colour = "disct")) +
  theme_classic()
# save the plot
ggsave("pairs.by.district.pdf",
       plot = pairs.by.district
       )
#
# END ---

# ---- point.pattern ----
# Point pattern analysis
# To confirm whether agglomeration exists or not.
# If exist, we need to consider spatial effect
# For the point pattern analysis the following sites
# might help.
# https://qiita.com/ishiijunpei/items/4234d78eacf2e7e7b8ef
# https://eburchfield.github.io/files/Point_pattern_LAB.html


# pick up points' location
hh.2010.sub.point <- 
  hh.2010.sub %>% 
  dplyr::select(lon, lat) 

# transform the point data into ppp-format data
hh.2010.sub.point.ppp <- 
  ppp(hh.2010.sub.point$lon,
      hh.2010.sub.point$lat,
      c(hh.2010.sub.point$lon%>%min(),hh.2010.sub.point$lon%>%max()),
      c(hh.2010.sub.point$lat%>%min(),hh.2010.sub.point$lat%>%max())
      )

# plot contour with density
# The results indicates that there are three agglomerated areas.
# It is likely that we need to consider spatial influence when
# we build an inference model.
hh.2010.sub.point.ppp %>%
  density() %>%
  contour(add=T)
# plot an envelop plot
# The plot indicate degree of agglomeration of target points.
hh.2010.sub.point.ppp%>%envelope(Lest,nism=999)%>%plot()

#
# END ---



plot(st_geometry(nc_triangles),   col = viridisLite::viridis(nrow(nc_triangles)))

