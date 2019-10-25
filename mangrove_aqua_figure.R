#read data from excel
library(readxl)
man_aqua <- read_excel("G:/My Drive/00_PROJECTS/ODA/Research/E-4/Training/R-Training_E-4_Nagasaki/Mangrove_area/Camau_mangrove_aquaculture_area_1983_2014.xlsx")
names(man_aqua)
# graph ggplot2
library(ggplot2); library(gridExtra); library(ggthemes)
p = ggplot(data=man_aqua, aes(x=Year, y=`Mangrove_area(ha)`, fill= Year, label=("")))
p = p + geom_bar(stat="identity", fill = "Green")
p = p + geom_text(size=3, color="white", position=position_stack(vjust=0.5))
p = p + theme_economist_white() + theme(legend.position="None")+
  labs(x = "Year", 
       y="Area of mangrove (Unit:1,000ha)"
  )+
  labs(caption = "Source: Ca Mau Department of Statistic, 2014") +
  theme(plot.caption = element_text(size=12,
                                    family = "Times",
                                    face = "italic"),
        axis.text = element_text(size = 12)
  )
p