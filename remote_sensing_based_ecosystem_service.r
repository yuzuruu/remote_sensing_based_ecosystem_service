############################################################
# Ecosystem service estimation using remote sensing results
# 09th. October 2019 First launch
# **th. ****** **** Correction
#
#


# ---- load.library ----
library(rgdal)
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
  labs(x = "Total area", y = "Density", colour = "District") +
  theme_classic()
# save the density plot
ggsave("hh.2010.sub.density.pdf")
#
# END ---
