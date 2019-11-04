############################################################
# Ecosystem service estimation using remote sensing results
# 09th. October 2019 First launch
# **th. ****** **** Correction
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
# # save the table
# write.csv(summary.hh.2010.sub, "summary.hh.2010.sub.csv")

# wrapped histogram by district
hh.2010.sub %>% 
  ggplot(aes(x = total_area))+
  geom_histogram()+
  facet_wrap(~ disct) +
  theme_classic()
# # save the histogram
# ggsave("hh.2010.sub.hist.pdf")

# density plot by district
hh.2010.sub %>% 
  ggplot(aes(x = total_area, colour = disct))+
  geom_line(stat = "density", size = 1)+
  scale_color_viridis_d(option = "viridis", aesthetics = "colour") +
  labs(x = "Total area", y = "Density", colour = "District", ) +
  theme_classic()
# # save the density plot
# ggsave("hh.2010.sub.density.pdf")
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

##################################################################
##################################################################
##################################################################

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
  mutate(X = utm.xy.selected$lon,
         Y = utm.xy.selected$lat
           ) %>% 
  rename(lon = X,
         lat = Y
         )
#
## --- END ---

# ---- find.contradiction ----
# find contradictional samples using condition match
# Note
# After finding some candidate of the contradictional samples,
# we need to consider whether they match or not individually.
# 1. Those who do not use mangroves and use for some purposes.
# fuel
selected.id.011 <- 
  hh.2010.selected  %>%  
  dplyr::filter(use_man == 2 & fuel != 0) %>% 
  dplyr::select(id)
print(as.character(selected.id.011$id))
# construction
selected.id.012 <- 
  hh.2010.selected  %>%  
  dplyr::filter(use_man == 2 & construction != 0) %>% 
  dplyr::select(id)
print(as.character(selected.id.012$id))
# fishing
selected.id.013 <- 
  hh.2010.selected  %>%  
  dplyr::filter(use_man == 2 & fishing != 0) %>% 
  dplyr::select(id)
print(as.character(selected.id.013$id))
# resting
selected.id.014 <- 
  hh.2010.selected  %>%  
  dplyr::filter(use_man == 2 & resting != 0) %>% 
  dplyr::select(id)
print(as.character(selected.id.014$id))
# maching function (7 observations were found.)
selected.id.01 <- 
  union(selected.id.011, selected.id.012) %>% 
  union(selected.id.013) %>% 
  union(selected.id.014)
# 2. Those who use mangroves and do not use for any purposes.
selected.id.02 <- 
  hh.2010.selected  %>%  
  dplyr::filter(use_man == 1 & fuel == 0 & construction == 0 & fishing == 0 & resting == 0) %>% 
  dplyr::select(id)
print(as.character(selected.id.02$id)) # 43 observations were found
# pick up ids of contradictional observation
# Note
# After finding some candidate of the contradictional samples,
# we need to consider whether they match or not individually.
contradictional.id <- 
  dplyr::bind_rows(selected.id.01, selected.id.02) %>% 
  dplyr::arrange(id)


# omit samples excluding contradictional samples
# If there would not be any problems, let us omit the
# contradictional samples.
hh.2010.selected.02 <- 
  hh.2010.selected %>% 
  dplyr::filter(!(id %in% as.numeric(contradictional.id$id)))

#
## --- END ---

