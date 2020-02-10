############################################################
# Ecosystem service estimation using remote sensing results
# 09th. October 2019 First launch
# 14th. January 2020 revision
#
# Entire codes will be synchronized on github.
# https://github.com/yuzuruu/remote_sensing_based_ecosystem_service

# ---- load.library ----
# When a package has not been installed, install the packages,
# please use a command below and install that.
# install.packages()
library(rgdal)
library(deldir)
library(spatstat)
library(spdep)
library(ggimage)
library(ggmap)
library(ggsn)
library(ggrepel)
library(ggspatial)
library(GGally)
library(tidyverse)
library(viridis)
library(viridisLite)

# ---- read.data ----
# To download sf file, please refer to the following website.
# The sf file is much lighter than .shp and suitable for R.
# https://gadm.org/download_country_v3.html
#
# read the sf file
vnm.adm.00 <- readRDS("gadm36_VNM_0_sf.rds") # province
vnm.adm.01 <- readRDS("gadm36_VNM_1_sf.rds") # province
vnm.adm.02 <- readRDS("gadm36_VNM_2_sf.rds") # district
vnm.adm.03 <- readRDS("gadm36_VNM_3_sf.rds") # commune
vnm.adm <- vnm.adm.03
# Pick up data of Ca Mau province
# Somehow it does not work.
vnm.adm %>% dplyr::filter(NAME_1 == "Ca Mau")
# It works.
vnm.adm.cm.01 <- 
  vnm.adm %>% 
  dplyr::filter(GID_1 == "VNM.13_1")
# To overwrite boundaries of province
vnm.adm.cm.02 <- 
  vnm.adm.02 %>% 
  dplyr::filter(GID_1 == "VNM.13_1")
vnm.adm.cm.03 <- 
  vnm.adm.03 %>% 
  dplyr::filter(GID_1 == "VNM.13_1")

# read household survey data
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
                disct, 
                village,
                age,
                sex,
                edu_le,
                prov,
                total_area,
                total_inco,
                total_invest,
                check.status
  ) %>% 
  # Omit observations containing NAs
  na.omit() %>% 
  #Transform the values into factor
  dplyr::mutate(sex = factor(sex, levels = c(1,2)),
                edu_le = factor(edu_le, levels = c(1,2,3,5,6))
  )
# pick up target district including target communes
# evaluate how many target samples exist in the commues each
# consider sampling / survey procedure
find.district.fun <- 
  function(lon, lat){
    id.location <- sf::st_contains(vnm.adm.cm.02,
                                   st_point(c(lon,
                                              lat
                                   )
                                   ),
                                   sparse = FALSE
    ) %>% 
      grep(TRUE, .)  
    vnm.adm.cm.02$VARNAME_2[id.location]
  }

find.commune.fun <- 
  function(lon, lat){
    id.location <- sf::st_contains(vnm.adm.cm.03,
                                   st_point(c(lon,
                                              lat
                                   )
                                   ),
                                   sparse = FALSE
    ) %>% 
      grep(TRUE, .)  
    vnm.adm.cm.03$VARNAME_3[id.location]
  }
# add names of district and commune from sf() files
hh.2010.sub <- 
  hh.2010.sub %>% 
  mutate(., 
         district = map2(.$lon, .$lat, find.district.fun),
         commune = map2(.$lon, .$lat, find.commune.fun)
  ) %>% 
  unnest(cols = c("district", "commune"))
#
# END ---

# To check data set in detail
# write_csv(hh.2010.sub, "hh.2010.sub.csv")

# # ---- summary.statistics.and.histogram ----
# # Make a descriptive statistics
# summary.hh.2010.sub <- 
#   hh.2010.sub %>% 
#   group_by(disct, village) %>% 
#   summarise(n = n(),
#             min.total.area = min(total_area),
#             mean.total.area = mean(total_area),
#             median.total.area = median(total_area),
#             max.total.area = max(total_area),
#             sd.total.area = sd(total_area)
#   )
# # print to confirm summary table content
# print(summary.hh.2010.sub, n = Inf)
# # # save the table
# # write.csv(summary.hh.2010.sub, "summary.hh.2010.sub.csv")
# 
# # wrapped histogram by district
# hh.2010.sub %>% 
#   ggplot(aes(x = total_area))+
#   geom_histogram()+
#   facet_wrap(~ disct) +
#   theme_classic()
# # # save the histogram
# # ggsave("hh.2010.sub.hist.pdf")
# 
# # density plot by district
# hh.2010.sub %>% 
#   ggplot(aes(x = total_area, colour = disct))+
#   geom_line(stat = "density", size = 1)+
#   scale_color_viridis_d(option = "viridis", aesthetics = "colour") +
#   labs(x = "Total area", y = "Density", colour = "District", ) +
#   theme_classic()
# # # save the density plot
# # ggsave("hh.2010.sub.density.pdf")
# #
# # END ---


# ---- map.camau ----
# Draw a map of administrative boundaries and overwrap survey points
# We use OpenStreet map for base layer.
# We can use google map. It, however, requires API when we use that many times.
# Referring to the following website, we are able to switch 
# type of map.
# https://wiki.openstreetmap.org/wiki/Tile_servers
# To overwrite the boundaries of districts and ones of wards, please refer to the 
# following website.
# https://stackoverflow.com/questions/49497182/ggplot-create-a-border-overlay-on-top-of-map
#
# read Google API
# change the API code as you obtain from Google
source("../../r_project/map.key.r")
# set centroid using latitude and longitude
lat.center.camau <- c(8.75)
lon.center.camau <- c(105.05)
# Obtain satellite imagery
map.sat.vnm.cm.total.area <- 
  get_map(location = c(lon = lon.center.camau,
                       lat = lat.center.camau
  ), 
  maptype = "satellite",
  zoom = 10
  ) %>% 
  ggmap() +
  geom_sf(data = vnm.adm.cm.03,
          colour = "grey60",
          fill = "white",
          alpha = 0.05,
          inherit.aes = FALSE
          ) +
    xlim(104.7, 105.5) +
    ylim(8.55, 9) +
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
         fill = "Total area (Unit: Ha)",
         caption = "\U00a9 Google"
    ) +
    theme_minimal() +
  theme(
    legend.position = c(0.8,0.3),
    legend.title = element_text(colour = "white"),
    legend.text = element_text(colour = "white")
  ) +
    # adjust scalebar's preferences
    ggsn::scalebar(x.min = 105.0,
                   x.max = 105.4,
                   y.min = 8.56,
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

# # save the plot
# # comment out when not in use
# ggsave("map.sat.vnm.cm.total.area.pdf",
#        plot = map.sat.vnm.cm.total.area
#        )
#
### --- END ---

# ---- map.total.area ----
# colouring by total area
# total area
map.osm.vnm.cm.total.area <- 
ggplot(vnm.adm.cm.01) + 
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
          data = vnm.adm.cm.02
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
# # save the map
# ggsave("map.osm.vnm.cm.total.area.pdf" ,
#        plot = map.osm.vnm.cm.total.area
#        )

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
          data = vnm.adm.cm.02
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
# # save the map
# ggsave("map.osm.vnm.cm.total.inco.pdf" ,
#        plot = map.osm.vnm.cm.total.inco
# )
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
          data = vnm.adm.cm.02
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
# # save the map
# ggsave("map.osm.vnm.cm.total.invest.pdf" ,
#        plot = map.osm.vnm.cm.total.invest
# )

#
# END ---

# ---- pair plot ----
pairs.by.district <- 
  hh.2010.sub %>%
  dplyr::select(disct, total_area, total_inco, total_invest) %>% 
  ggpairs(aes_string(colour = "disct")) +
  theme_classic()
# # save the plot
# ggsave("pairs.by.district.pdf",
#        plot = pairs.by.district
#        )
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
st_(hh.2010.sub.point)
plot(delaunay(hh.2010.sub.point.ppp))


hh.2010.sub %>% 
  ggplot(aes(x = lon, y = lat)) +
  stat_density2d(aes(fill = ..density..),
                 geom = "raster",
                 contour = FALSE
                 ) +
  scale_fill_viridis(alpha = 0.5)





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


# # ---- barplot.mangrove ----
# # This is supplement o
# # read data
mangrove.area <-
  readxl::read_excel("../mangrove_area.xlsx")
# 2D barplot
# The 2D barplot is enough for our paper, I guess.
# If you need the 3D barplot,
barplot.mangrove.area <-
  mangrove.area %>%  # assign data to ggplot2 object
  ggplot(aes(x = Year, y = `mangrove_area (th.ha)`)) + # set a plot area
  geom_bar(stat = "identity",
           fill = "darkgreen" # fix bars' color.
           ) +
  labs(x = "Year", # change label (x axis)
       y = "Area of mangrove (Unit: 1,000 ha)" # change label (y axis)
       ) +
  labs(caption = "Nguon: Tong cuc lam nghiep, 2014") + # add a caption
  theme_economist_white() + # set a theme
  # adjust preference of caption and axis labels
  theme(plot.caption = element_text(size = 15,
                                    family = "Times",
                                    face = "italic"),
        axis.text = element_text(size = 12)
        )
# # # save
# # ggsave("barplot.mangrove.area.pdf", plot = barplot.mangrove.area)


# ---- Distance-based neibours ----
# To make a model, we need to consider spatial pattern of observations.
# Here, while applying spdep::dnearneigh function, 
# we check whether points exists within a certain circle.
# Namely, we donfirm whether some observations are exist or not.
# References
# https://stackoverflow.com/questions/48152269/make-a-spatialpointsdataframe-with-sf-the-fast-way
# http://fusion0202.blog.jp/archives/602289.html
# https://www.r-bloggers.com/plotting-spatial-neighbors-in-ggmap/

# make a subset including longitude and latitude
hh.2010.sub.latlon <- 
  hh.2010.sub %>% 
  dplyr::select(lon, lat)  
# transform the selected data into sf-formatted data
# with projection information
# 
hh.2010.sub.latlon.sf <- 
  sf::st_as_sf(hh.2010.sub.latlon, 
               coords = c("lon", "lat"),  
               crs = "+proj=longlat +datum=WGS84 +no_defs"
               ) 

# compute list of neighbourhood based on distance
# Unit of d1 and d2 is DEGREE. Distance between d1 and d2 differs depending on location.
# The function compute the distance while referring to the degree-based location.
# Eg.
# d2 is 0.05, indicating 0.05 times degree between the point 
# (Approx. 5.5km)
# So the code below compute whether points exist within 5.5km-radius circle (11km in diameter).
# Of course, we can ad
hh.2010.sub.latlon.nb <- 
  dnearneigh(hh.2010.sub.latlon.sf,
             d1 =  0, # 
             d2 = 0.05, # radius of checking circle is 0.05 (Approx. 5.5km) 
             longlat = TRUE
             )

# transform the result  
# Spatial analysis packages often do not accept tidyverse data.
# This time, by transforming into normal one, we apply the
# function below.
hh.2010.sub.latlon.cent <- as.data.frame(hh.2010.sub.latlon)
colnames(hh.2010.sub.latlon.cent) <- c("lon","lat")

# a function to display list of positions of neighbourhood points
nb_to_df <- function(nb, coords){
  x <- coords[, 1]
  y <- coords[, 2]
  n <- length(nb)
  
  cardnb <- card(nb)
  i <- rep(1:n, cardnb)
  j <- unlist(nb)
  return(data.frame(x=x[i], xend=x[j],
                    y=y[i], yend=y[j]))
}

# obtain list of neighbourhood points
hh.2010.sub.latlon.df <- 
  nb_to_df(hh.2010.sub.latlon.nb, 
           hh.2010.sub.latlon.cent
           )

# plot the map of total investment with neighbourhood pattern
# Points' colors are based on amount of total investiment.
# The map shows that neighbourhood relationship is defined in dense area.
# In contrast, in sparse area, the relationships is not defined.
# At least, the results indicate that we need to consider spatial autocorrelations.
map.osm.vnm.cm.total.invest.nb <- 
  map.osm.vnm.cm.total.invest + 
  # Overwrap survey point using survey results
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend),
               size = 0.5, 
               data=hh.2010.sub.latlon.df
               ) 

# # save the map
# ggsave("map.osm.vnm.cm.total.invest.nb.pdf",
#        plot = map.osm.vnm.cm.total.invest.nb
#        )

#
# END ---

# ---- official.statistics.population
# read data
# The data includes population by gender between 1976 and 2015.
# Data in subsequent period should be collected.
camau.official <- readxl::read_excel("indicators_camau.xlsx",
                                     sheet = "population"
                                     )
# reshape the data set
camau.population <- 
  camau.official %>% 
  dplyr::mutate(population.total = as.numeric(population_total),
         population.male = as.numeric(population_male),
         population.female = as.numeric(population_female)
         ) %>% 
  dplyr::select(year, 
                district, 
                area, 
                population.total, 
                population.male, 
                population.female
                )
# line plot
# We'd explain separation of district.
# some lines evidently changes dramatically.
camau.population %>% 
  dplyr::select(-area) %>% 
  ggplot(aes(x = year, y = population.total, fill = district)) + 
  geom_line() +
  geom_point(shape = 23) +
  theme_classic()

# 
# END ---




# ---- barplot.aquaculture.mangrove.area ---- 
# read data
cm.mangrove.aqua.1983.2014 <- readxl::read_excel("../Mangrove_area/Camau_mangrove_aquaculture_area_1983_2014.xlsx")
# reshape the data for convenience
cm.mangrove.aqua <- 
  cm.mangrove.aqua.1983.2014 %>% 
  dplyr::rename(year = Year,
                Mangrove = `Mangrove_area(ha)`,
                Aquaculture = `Aquaculture_area(ha)`
  ) %>% 
  tidyr::gather(key = landuse.type,
                value = ha, Mangrove, Aquaculture
  ) %>% 
  dplyr::mutate(year = as.factor(year))
# draw a barplot
barplot.cm.mangrove.aqua <- 
  cm.mangrove.aqua %>% 
  # set a plot area
  ggplot(aes(x = year, # x axis 
             y = ha,  # y axis
             fill = landuse.type # factors to fill color
  )
  ) +
  # set type of figure
  # In this case, we draw a bar plot 
  geom_bar(stat = "identity", 
           position = "dodge", # adjust position of bars
           colour = "grey60" 
  ) +
  # adjust color of bars by valuable
  # In detail of colors' name, please refer to the following website.
  # http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  scale_fill_manual(values = c("skyblue",  # 1st. valuable (Aquaculture)
                               "darkgreen" # 2nd. Valuable (Mangrove)
  )
  ) +
  # change axes labels and legend title
  labs(x = "Year", 
       y = "Area (Unit: ha)", 
       fill = "Land use type"
  ) +
  # set theme
  theme_classic() +
  theme(
    legend.position = c(0.4, 0.8)
  )

# Another option of barplot with points
cm.mangrove.aqua.02 <- 
  cm.mangrove.aqua.1983.2014 %>% 
  dplyr::rename(year = Year,
                Mangrove = `Mangrove_area(ha)`,
                Aquaculture = `Aquaculture_area(ha)`
  ) %>% 
  dplyr::mutate(year = as.factor(year))

# draw a barplot with line and poing
# To connect the points with line, we need to
# add "group = 1" at aesthetic argument.
# In detail, please refer to the following page.
# https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation
barplot.cm.mangrove.aqua.02 <- 
  cm.mangrove.aqua.02 %>% 
  ggplot() +
  geom_bar(aes(x = year, # x axis 
               y = Aquaculture
               ),
           stat = "identity", 
           position = "dodge", # adjust position of bars
           colour = "grey60",
           fill = "skyblue"
  ) +
  geom_line(aes(x = year, 
                y = Mangrove, 
                group = 1 #Here to connect the point with line
                ),
            stat = "identity", 
            position = "identity",
            size = 1
  ) +
  geom_point(data = cm.mangrove.aqua.02,
             aes(x = year, 
                 y = Mangrove
                 ),
             stat = "identity", 
             position = "identity",
             fill = "darkgreen", 
             size = 3
  ) +
  labs(x = "Year", 
       y = "Area (Unit: ha)", 
       fill = "Land use type",
       shape = 3
  ) +
  # set theme
  theme_classic() +
  theme(
    legend.position = c(0.4, 0.8)
  )
# confirm the result
print(barplot.cm.mangrove.aqua.02)
# # save the plot
# # Please comment out when not in use.
# ggsave("barplot.cm.mangrove.aqua.02.pdf",
#        plot = barplot.cm.mangrove.aqua.02
#        )
#
#
##--- END ---
# selected data and consideration in detail
# NOTE:
# load required libraries in advance
#
# ---- read.selected.data ----
# To transform datum, please refer to the following page.
# https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r
# read data
hh.2010.selected <- 
  readxl::read_excel("../Household_survey_data/household_survey_results_for_revision.xlsx",
                     sheet = "survey.2010.selected",
                     range = "A02:CS224",
                     col_names = TRUE
                     )
# Transform the UTM into lonlat
utm.xy.selected <- 
  hh.2010.selected %>% 
  dplyr::select(X, Y) %>% 
  SpatialPoints(., proj4string=CRS("+proj=utm +zone=48 +datum=WGS84")) %>% 
  spTransform(., CRS("+proj=longlat +datum=WGS84")) %>% 
  as_tibble() %>% 
  mutate(lon = X,
         lat = Y
         )
# replace UTM location and the transformed location 
hh.2010.selected <- 
  hh.2010.selected %>% 
  mutate(lon = utm.xy.selected$lon,
         lat = utm.xy.selected$lat
           ) %>% 
  rename(lon = X,
         lat = Y
         )
#
## --- END ---

# # ---- find.contradiction ----
# # NOTE on 5th. November 2019
# # The selected data is not sceptical. The contradiction of
# # answers results from limitation of interview survey.
# # This time, we treat the data as "TRUE" data.
# # For reference, I will leave the code.
# # find contradictional samples using condition match
# # Note
# # After finding some candidate of the contradictional samples,
# # we need to consider whether they match or not individually.
# # 1. Those who do not use mangroves and use for some purposes.
# # fuel
# selected.id.011 <- 
#   hh.2010.selected  %>%  
#   dplyr::filter(use_man == 2 & fuel != 0) %>% 
#   dplyr::select(id)
# print(as.character(selected.id.011$id))
# # construction
# selected.id.012 <- 
#   hh.2010.selected  %>%  
#   dplyr::filter(use_man == 2 & construction != 0) %>% 
#   dplyr::select(id)
# print(as.character(selected.id.012$id))
# # fishing
# selected.id.013 <- 
#   hh.2010.selected  %>%  
#   dplyr::filter(use_man == 2 & fishing != 0) %>% 
#   dplyr::select(id)
# print(as.character(selected.id.013$id))
# # resting
# selected.id.014 <- 
#   hh.2010.selected  %>%  
#   dplyr::filter(use_man == 2 & resting != 0) %>% 
#   dplyr::select(id)
# print(as.character(selected.id.014$id))
# # maching function (7 observations were found.)
# selected.id.01 <- 
#   union(selected.id.011, selected.id.012) %>% 
#   union(selected.id.013) %>% 
#   union(selected.id.014)
# # 2. Those who use mangroves and do not use for any purposes.
# selected.id.02 <- 
#   hh.2010.selected  %>%  
#   dplyr::filter(use_man == 1 & fuel == 0 & construction == 0 & fishing == 0 & resting == 0) %>% 
#   dplyr::select(id)
# print(as.character(selected.id.02$id)) # 43 observations were found
# # pick up ids of contradictional observation
# # Note
# # After finding some candidate of the contradictional samples,
# # we need to consider whether they match or not individually.
# contradictional.id <- 
#   dplyr::bind_rows(selected.id.01, selected.id.02) %>% 
#   dplyr::arrange(id)
# # save observations including contradiction
# hh.2010.selected.contradiction <- 
#   hh.2010.selected %>% 
#   dplyr::filter(id %in% as.numeric(contradictional.id$id))
# write.csv(hh.2010.selected.contradiction, "hh.2010.selected.contradiction.csv")
# 
# # omit samples excluding contradictional samples
# # If there would not be any problems, let us omit the
# # contradictional samples.
# hh.2010.selected.02 <- 
#   hh.2010.selected %>% 
#   dplyr::filter(!(id %in% as.numeric(contradictional.id$id)))
# 
# #
# ## --- END ---



# # WARNING
# # THIS PROCESS NEEDS COMPUTATION PERIOD.
# # COMMENT OUT WHEN NOT IN USE.
# # Make maps to detect farmers' / fishermen's using areas
# # By having them draw an area (circle, rectangle), we are able to 
# # obtain exact information on their land use.
# # read Google API
# # change the API code as you obtain from Google
# ---- area.map ----
source("../../r_project/map.key.r")

# # set common arguments
# # When we adjust settings of the map, adjust the following argument.
# # ---- set.common.arguments ----
# cellsize  <-  25 # size of cell (unit: meter (m))
# alpha  <-  0.1 # alpha channel (0.1 should be better.)
# zoom <-  17 # zooming 
# # location of target and their id
# lon.lat.id <- list(center.lon = hh.2010.sub$lon,
#                    center.lat = hh.2010.sub$lat,
#                    id = c(1:nrow(hh.2010.sub))
# )
# #
# ## --- END ---
# 
# # function to make a boundary box
# # Original code is below.
# # https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
# # EPSG by google can be obtained from below.
# # https://colauttilab.github.io/EcologyTutorials/mapping.html
# # ---- ggmap.bbox.fun ----
# ggmap.bbox.fun <- function(sat.map) {
#   if (!inherits(sat.map, "ggmap")) stop("map must be a ggmap object")
#   # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
#   # and set the names to what sf::st_bbox expects:
#   map_bbox <- setNames(unlist(attr(sat.map, 
#                                    "bb"
#                                    )
#                               ), 
#                        c("ymin", 
#                          "xmin", 
#                          "ymax", 
#                          "xmax"
#                          )
#                        )
#   # Coonvert the bbox to an sf polygon, transform it to 3857, 
#   # and convert back to a bbox (convoluted, but it works)
#   # st_as_sfc requires CRS. Google maps obtained from get_map() has no CRS info.
#   # We need to set the temporal CRS first. Then we transform the temporal CRS
#   # into real one (3857).
#   bbox.3857 <- 
#     map_bbox %>% 
#     st_bbox(crs = 4326) %>%
#     st_as_sfc %>% 
#     st_transform(crs = 3857) %>%
#     st_bbox()
#   # Overwrite the bbox of the ggmap object with the transformed coordinates 
#   # Names below can be obtained using str(map) function
#   attr(sat.map, "bb")$ll.lat <- bbox.3857["ymin"]
#   attr(sat.map, "bb")$ll.lon <- bbox.3857["xmin"]
#   attr(sat.map, "bb")$ur.lat <- bbox.3857["ymax"]
#   attr(sat.map, "bb")$ur.lon <- bbox.3857["xmax"]
#   sat.map
# }
# #
# ## --- END ---
# 
# # obtain a map from Google
# # Internet connection and Google API are necessary.
# # ---- sat.grid.fun ----
# sat.grid.fun <- function(center.lon, center.lat, id){
#   sat.map <- 
#     get_map(location = c(lon = center.lon,
#                        lat = center.lat
#                        ),
#           maptype = "satellite",
#           zoom = zoom
#           ) 
#     # make grids
#     # 
#     # https://tsukubar.github.io/r-spatial-guide/simple-feature-for-r.html
#     map.grid <- 
#       tibble::data_frame(
#         id = seq(1,2),
#         lon = as.numeric(unlist(attr(sat.map, which = "bb"))[c(2,4)]),
#         lat = as.numeric(unlist(attr(sat.map, which = "bb"))[c(1,3)])
#       ) %>% 
#       st_as_sf(coords = c("lon","lat"),
#                crs = 4326
#       ) %>% 
#       st_transform(crs = 3857) %>% 
#       st_bbox() %>% 
#       sf::st_make_grid(.,
#                        cellsize = cellsize # size of grid. Unit is metre (m)
#       ) 
#     
#     # make a ggplot-object map from the obtained map and
#     # overlay the grid on the map
#     sat.map <- ggmap.bbox.fun(sat.map)
#     
#     ggmap(sat.map) + 
#       coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
#       geom_sf(data = map.grid, 
#               fill = "white", 
#               colour = "white",
#               alpha = alpha, 
#               lwd = 0.01,
#               inherit.aes = FALSE
#               ) +
#       labs(x = "Longitude",
#            y = "Latitude",
#            title = id,
#            subtitle = paste("Size of square is", cellsize,"m*", cellsize, "m")
#            ) +
#       theme_minimal()
#     }
# #
# ## --- END ---
# 
# # save the maps as a pdf file
# # comment out when not in use.
# # ---- target.landuse.map ----
# # pdf("target_landuse_survey_map.pdf")
# # lon.lat.id %>%
# #   pmap(sat.grid.fun)
# # dev.off()
# #
# #
# ## --- END ---
# 


# Ca Mau province map by commune / ward
# ---- read.sf.data ----
# province
vnm.adm.cm.01 <- 
  vnm.adm.01 %>% 
  dplyr::filter(GID_1 == "VNM.13_1")
# To overwrite boundaries of province
vnm.adm.cm.02 <- 
  vnm.adm.02 %>% 
  dplyr::filter(GID_1 == "VNM.13_1") %>% 
  mutate(centroid = map(.$geometry, st_centroid))
vnm.adm.cm.03 <- 
  vnm.adm.03 %>% 
  dplyr::filter(GID_1 == "VNM.13_1") %>% 
  mutate(centroid = map(.$geometry, st_centroid))
#
## --- END ---


# ---- compute.centroid.by.commune ----
# How to use do.call(), refer to the following webpage.
# http://www.okadajp.org/RWiki/?%E3%83%AA%E3%82%B9%E3%83%88Tips%E5%A4%A7%E5%85%A8#kd290b33
# make a function to do that at a time
# Warning
# To avoid malfunction to use tidyverse(), we make the function
# by base() package.
st_centroid.fun <- function(geometry, VARNAME_3){
    sf::st_centroid(geometry) %>% # compute centroid
    base::do.call("rbind", .) %>% 
    base::cbind(., VARNAME_3) %>% 
    base::data.frame()
  }
# compute centroid
vnm.adm.cm.03.centroid <- 
  st_centroid.fun(vnm.adm.cm.03$geometry, 
                  vnm.adm.cm.03$VARNAME_3
                  )
# rename the data containing centroid
colnames(vnm.adm.cm.03.centroid) <- 
  c("lon",
    "lat",
    "VARNAME_3"
    )
# convert data type
vnm.adm.cm.03.centroid <- 
  vnm.adm.cm.03.centroid %>% 
  as_tibble() %>% 
  mutate(lon = as.numeric(as.character(lon)),
         lat = as.numeric(as.character(lat))
         )
# merge the centroid data and original sf data
vnm.adm.cm.03 <-left_join(vnm.adm.cm.03, vnm.adm.cm.03.centroid, by = "VARNAME_3")
# compute the centroid of Ca Mau province to download google satellite imagery
camau.center <- 
  st_centroid(vnm.adm.cm.01) %>% 
  unlist() %>% 
  data.frame() %>% 
  setNames("geometry")
cm.center.lon <- as.numeric(as.character(camau.center$geometry[11]))
cm.center.lat <- as.numeric(as.character(camau.center$geometry[12]))
# #
# ## --- END ---

# ---- map.sat.cm.by.commune ----
# download satellite image
sat.map.cm <-
  get_map(location = c(lon = cm.center.lon,
                       lat = cm.center.lat
                       ),
          maptype = "satellite",
          zoom = 9
          )
# add communes' boundaries and names to the satellite imagery
map.sat.cm.commune.name.whole <- 
  ggmap(sat.map.cm) +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = vnm.adm.cm.03,
          color = "white",
          fill = "transparent",
          size=0.125,
          inherit.aes = FALSE
  ) +
  geom_sf(data = vnm.adm.cm.02,
          color = "white", 
          fill = "transparent", 
          size=1,
          inherit.aes = FALSE
  ) +
  geom_text_repel(data = vnm.adm.cm.03, 
                  label = vnm.adm.cm.03$VARNAME_3,
                  color = "white",
                  size = 1.5,
                  na.rm = TRUE
                  ) +
  labs(x = "Longitude", y = "Latitude") +
  xlim(104.7, 105.7) +
  ylim(8.55, 9.55) +
  ggsn::scalebar(x.min = 105.4,
                 x.max = 105.60,
                 y.min = 8.60,
                 y.max = 8.65, 
                 dist_unit = "km",
                 dist = 20, 
                 st.size = 3,
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
ggsave("map.sat.cm.commune.name.whole.pdf", 
       plot = map.sat.cm.commune.name.whole
       )
# #
# ## --- END ---

# pick up target district including target communes
# evaluate how many target samples exist in the commues each
# consider sampling / survey procedure
# When 5 observations should be collected from each commune,
# n. will be as follows:
# n. of observed in 2010 and still exist (available to be panel data): 53
# n. of observed in 2010 and NOT exist (so should be added to): 22
# n. of NOT observed in 2010 and to be observed: 40
# 115 in total
# Note
# 6 per commune: 148
# 7 per commune: 153
# 8 per commune: 192
# ---- n.of.target ----
communes.observed.2010 <- 
  hh.2010.sub %>% 
  dplyr::filter(district %in% c("Phu Tan",
                                "Nam Can",
                                "Ngoc Hien")) %>% 
  dplyr::filter(check.status == 1) %>% 
  dplyr::group_by(district, commune) %>% 
  dplyr::summarize_at(vars(check.status), funs(length))
communes.to.be.observed <- 
  vnm.adm.cm.03 %>% 
  st_drop_geometry() %>% 
  dplyr::filter(GID_2 %in% c("VNM.13.6_1", # Phu Tan
                             "VNM.13.5_1", # Ngok Hien
                             "VNM.13.4_1" # Nam Can
                             )
                ) %>% 
  dplyr::filter(VARNAME_3 %in% setdiff(levels(factor(.$VARNAME_3)), 
                                       communes.observed.2010$commune)
                ) %>% 
  dplyr::select(NAME_2,VARNAME_3) %>% 
  mutate(check.status = 0) %>% 
  dplyr::rename(district = NAME_2,
                commune = VARNAME_3,
                check.status = check.status
                ) %>% 
  as_tibble()
communes.target <- 
  bind_rows(communes.observed.2010, 
            communes.to.be.observed
            ) %>% 
  mutate(additional.observation = 8-check.status) %>% 
  mutate_at(vars(additional.observation), 
            funs(ifelse(additional.observation<0,
                        0,
                        .
                        )
                 )
            ) 

table.communes.target <- 
  knitr::kable(communes.target, 
               format = "markdown", 
               caption = "Target quadrats"
               )

# #
# ## --- END ---




# Map of target communes with maps of Vietnam and
# Ca Mau province
#
# ---- map.sat.cm.target.district.commune ----
# obtain a centroid of the 3 communes
# The centroid is for obtaining Google satellite imagery.
# and names of commune.
# sf() added centroids by commune
vnm.adm.cm.03.three.communes <- 
  vnm.adm.cm.03 %>% 
  dplyr::filter(GID_2 %in% c("VNM.13.6_1", # Phu Tan
                             "VNM.13.5_1", # Ngok Hien
                             "VNM.13.4_1" # Nam Can
  )
  ) 
# sf() added centroids by district
vnm.adm.cm.02.three.communes <- 
  vnm.adm.cm.02 %>% 
  dplyr::filter(GID_2 %in% c("VNM.13.6_1", # Phu Tan
                             "VNM.13.5_1", # Ngok Hien
                             "VNM.13.4_1" # Nam Can
  )
  ) 
# centroid of Ca Mau province
cm.center.three.communes <- 
  vnm.adm.cm.03 %>% 
  dplyr::filter(GID_2 %in% c("VNM.13.6_1", # Phu Tan
                             "VNM.13.5_1", # Ngok Hien
                             "VNM.13.4_1" # Nam Can
  )
  ) %>%
  sf::st_union() %>% 
  sf::st_centroid() %>% 
  base::do.call("rbind", .) %>% 
  base::data.frame()
colnames(cm.center.three.communes) <- c("lon", "lat")

# Data obtained in 2020 belonging to the three communes
hh.2010.sub.three.communes<-
  hh.2010.sub %>% 
  dplyr::filter(district %in% c("Nam Can","Ngoc Hien","Phu Tan"))

# obtain a satellite imagery from Google maps
sat.map.cm.three.communes <-
  get_map(location = c(lon = cm.center.three.communes$lon,
                       lat = cm.center.three.communes$lat
  ),
  maptype = "satellite",
  zoom = 10
  )
# map with target communes on satellite imagery
map.sat.cm.commune.name.three.communes <- 
  ggmap(sat.map.cm.three.communes) +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = vnm.adm.cm.03.three.communes,
          color = "white",
          fill = "transparent",
          size=0.125,
          inherit.aes = FALSE
  ) +
  geom_sf(data = vnm.adm.cm.02.three.communes,
          color = "white",
          fill = "transparent",
          size=1,
          inherit.aes = FALSE
  ) +
  geom_point(data = hh.2010.sub.three.communes,
             aes(x = lon, 
                 y = lat,colour = as.factor(check.status)
                 )
             ) +
  # We do not display points unavailable to perform survey
  # Because of development and other reasons, we skip the 
  # targets obtained in 2010
  scale_colour_manual(values = 
                        c("transparent",
                          "yellow" # points collected in 2010 and existing in 2020
                          )
                      ) +
  # communes' names
  geom_text_repel(data = vnm.adm.cm.03.three.communes, 
                  label = vnm.adm.cm.03.three.communes$VARNAME_3,
                  color = "white",
                  size = 2,
                  na.rm = TRUE
  ) +
  labs(x = "Longitude", y = "Latitude") +
  xlim(104.7, 105.30) +
  ylim(8.55, 9.15) +
  theme(
    legend.position = "none"
  ) +
  ggsn::scalebar(x.min = 105.2,
                 x.max = 105.3,
                 y.min = 8.57,
                 y.max = 8.60, 
                 dist_unit = "km",
                 dist = 10, 
                 st.size = 3,
                 st.dist = 0.3,
                 height = 0.25,
                 model = "WGS84", 
                 transform = TRUE,
                 location = "bottomright",
                 box.fill = c("grey30", "white"), # left and right
                 box.color = "white",
                 st.color = "white"
  ) 

# map.whole.republic
map.whole.republic <- 
  ggplot(vnm.adm.00) +
  geom_sf(fill = "white") +
  # Ha Noi
  geom_point(aes(x = 105.80, # Ha Noi
                 y = 21.0,
  ),
  size = 5
  ) +
  annotate("text", 
           x = 105.80, 
           y = 21.5, 
           label = "Ha Noi"
           ) +
  # HCMC
  geom_point(aes(x = 106.50, # HCMC
                 y = 10.80
  ),
  size = 5
  ) +
  annotate("text", 
           x = 104.80, 
           y = 11.2, 
           label = "Ho Chi Minh City"
           ) +
  # Ca Mau
  geom_point(aes(x = 105.14, # Ca Mau
                 y = 9.18
  ),
  size = 5
  ) +
  annotate("text", 
           x = 106.2, 
           y = 9.18, 
           label = "Ca Mau"
           ) +
  # Vietnam
  annotate("text", 
           x = 105.00, 
           y = 15.5, 
           label = "Vietnam", 
           size = 10) +
  theme_void()

# map.camau.by.district
# compute centroid by district
vnm.adm.cm.02.centroid <- 
  vnm.adm.cm.02 %>% 
  st_centroid()
vnm.adm.cm.02.centroid <- 
  do.call("rbind",vnm.adm.cm.02.centroid$geometry)
colnames(vnm.adm.cm.02.centroid) <- c("lon","lat")
# combine the centroid with sf() file
vnm.adm.cm.02<- 
  cbind(vnm.adm.cm.02, 
        vnm.adm.cm.02.centroid
        )
# plot a white map of Ca Mau province
map.cm.district <- 
  ggplot(vnm.adm.cm.02) + 
  geom_sf(fill = "transparent", colour = "white") +
  geom_text_repel(data = vnm.adm.cm.02, 
                  aes(x = lon, y = lat),
                  label = vnm.adm.cm.02$VARNAME_2,
                  color = "white",
                  size = 2,
                  na.rm = TRUE
  ) +
  theme_void()

# map.whole.republic
map.whole.republic <- 
  ggplot(vnm.adm.00) +
  geom_sf(fill = "transparent", colour = "white") +
  # Ha Noi
  # point
  geom_point(aes(x = 105.80, # Ha Noi
                 y = 21.0,
  ),
  size = 3,
  colour = "white"
  ) +
  # text
  annotate("text", 
           x = 108.10, 
           y = 21.0, 
           label = "Ha Noi", 
           colour = "white", 
           size = 2.5
           ) +
  # HCMC
  # point
  geom_point(aes(x = 106.50, # HCMC
                 y = 11.00
  ),
  size = 3,
  colour = "white"
  ) +
  # text
  annotate("text", 
           x = 108.00, 
           y = 12.0, 
           label = "Ho Chi Minh City", 
           colour = "white", 
           size = 2.5
           ) +
  # Ca Mau
  # point
  geom_point(aes(x = 105.14, # Ca Mau
                 y = 9.18
  ),
  size = 3,
  colour = "white"
  ) +
  # text
  annotate("text", 
           x = 107.85, 
           y = 9.18, 
           label = "Ca Mau", 
           colour = "white", 
           size = 2.5
           ) +
  theme_void()

# map.camau.by.district
# compute centroid by district
vnm.adm.cm.02.centroid <- 
  vnm.adm.cm.02 %>% 
  st_centroid()
vnm.adm.cm.02.centroid <- 
  do.call("rbind",vnm.adm.cm.02.centroid$geometry)
colnames(vnm.adm.cm.02.centroid) <- c("lon","lat")
# combine the centroid with sf() file
vnm.adm.cm.02<- cbind(vnm.adm.cm.02, 
                      vnm.adm.cm.02.centroid
                      )

# plot a white map of Ca Mau province
map.cm.district <- 
  ggplot(vnm.adm.cm.02) + 
  geom_sf(fill = "transparent", colour = "white") +
  geom_text_repel(data = vnm.adm.cm.02, 
                  aes(x = lon, y = lat),
                  label = vnm.adm.cm.02$VARNAME_2,
                  color = "white",
                  size = 2,
                  na.rm = TRUE
  ) +
  theme_void()
df_plots <- 
  data_frame(x = c(105.075, 105.25), 
             y = c(9.05, 9.05), 
             width = 0.2,
             height = 0.2, 
             plot = list(map.whole.republic,
                         map.cm.district
                         )
             )

# map.sat.cm.target.district.commune
# Combine all the four maps above
map.sat.cm.target.district.commune <- 
  map.sat.cm.commune.name.three.communes + 
  ggimage::geom_subview(aes(x = x,
                            y = y,
                            subview = plot, 
                            width = width,
                            height = height
                            ), 
                        data = df_plots
                       ) +
  # vertical line
  geom_segment(aes(x = 105.02, 
                   xend = 105.02, 
                   y = 9.15, 
                   yend = 9.00
  ),
  colour = "white",
  size = 0.5
  ) +
  # 
  geom_segment(aes(x = 105.02, 
                   xend = 105.05, 
                   y = 9.00, 
                   yend = 8.95
  ),
  colour = "white",
  size = 0.5
  ) +
  # horizontal line
  geom_segment(aes(x = 105.05, 
                   xend = 105.3, 
                   y = 8.95, 
                   yend = 8.95
  ),
  colour = "white",
  size = 0.5
  ) +
  # vertical line inside of submap area
  geom_segment(aes(x = 105.175, 
                   xend = 105.175, 
                   y = 9.15, 
                   yend = 8.98
  ),
  colour = "white",
  size = 0.5
  ) +
  # text (Viennam) 
  annotate("text", 
           x = 105.10, 
           y = 9.15, 
           label = "Vietnam", 
           colour = "white", 
           size = 4
  ) +
  # text (Ca Mau Province)
  annotate("text", 
           x = 105.25, 
           y = 9.15, 
           label = "Ca Mau", 
           colour = "white", 
           size = 4
  ) 

map.sat.cm.target.district.commune

# Save the map
# Comment out when not in use
# ggsave("map.sat.cm.target.district.commune.pdf",
#        plot = map.sat.cm.target.district.commune
#        )
# #
# ## --- END ---



