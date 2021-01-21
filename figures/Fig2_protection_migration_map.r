library(ggplot2)
library(stringr)
library(rgdal)
library(plyr)
library(maptools)
library(rgeos)

source("layout.r")

data <- read.csv("../results/data_complete_reduced_main_table.csv")
countryResults <- read.csv("../results/country_results_m011_mc3_0.csv")

cls <- readOGR(dsn="../data/gis/cls_p32.shp", layer="cls_p32")
cls@data$id = rownames(cls@data)
cls.points = fortify(cls, region="id")
cls_df = join(cls.points, cls@data, by="id")
cls_df <- cls_df[,c("long","lat","order","piece","group","id","locationid")]

countries <- readOGR(dsn="../data/gis/country.shp", layer="country")
countries@data$id = rownames(countries@data)
countries.points = fortify(countries, region="id")
countries_df = join(countries.points, countries@data, by="id")

plotData <- join(cls_df, data, by = c("locationid"), type = "inner")
names(countryResults)[names(countryResults)=="ISO3166"] <- "locationid"
names(countryResults)[names(countryResults)=="Never.protect...."] <- "npp"

plotDataNoResponse <- plotData[(plotData$protect_mc3_0_Migration==0 & plotData$migration_submergence_mc3_0_Migration==0),]
plotDataResponse <- plotData[(plotData$protect_mc3_0_Migration>0 | plotData$migration_submergence_mc3_0_Migration>0),]

plotCountryData <- countries_df
plotCountryData <- join(countries_df, countryResults, by = c("locationid"), type = "left")

plotCountryData[is.na(plotCountryData$npp),]$npp <- 100

legend <- theme(
    legend.position="bottom", 
    legend.direction = "horizontal",
    legend.box="horizontal",
    legend.text=element_text(size=12),
    legend.title=element_text(size=16),
    legend.key.width = unit(2.75, "cm")
)

plot1 <- ggplot(data=plotCountryData,aes(long,lat,group=group)) + 
    geom_polygon(aes(fill=npp)) + 
    geom_path(lwd=0.3, color="gray60") +
    theme_minimal(20) + 
    scale_fill_gradientn(colours = c("grey20", "gray45", "white"), space = "Lab", limits=c(0,100), expand=c(0,0), name = "Percentage of coastline with non-positive NPV over all scenarios")

plotDataResponse[(plotDataResponse$protect_mc3_0_Migration>0 & plotDataResponse$protect_mc3_0_Migration<100),]$protect_mc3_0_Migration <- 50
plotDataResponse$protect_mc3_0_Migration <- as.factor(plotDataResponse$protect_mc3_0_Migration)
levels(plotDataResponse$protect_mc3_0_Migration)[levels(plotDataResponse$protect_mc3_0_Migration)=="50"] <- "1-99"

plot2 <- plot1 + 
    geom_path(data=plotDataResponse,aes(long,lat, group=group, colour=protect_mc3_0_Migration),lwd=0.6) + 
    scale_color_manual(values = c("red", "gold1", "darkgreen"), name = "Percentage of scenarios with positive NPV of protection") + 
    legend + 
    axis +
    panel +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0.02,0)) + 
    labs(x=NULL, y=NULL, title=NULL) +
    theme(plot.margin = rep(unit(0,"null"),4)) +  
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5),
           fill = guide_colourbar(title.position="top", title.hjust = 0.5))

plot3 <- plot2 + 
    geom_path(data=plotDataNoResponse,aes(long,lat, group=group),lwd=0.6,color="grey60")

ggsave("./eps/Fig2_map_protect_migrate.eps", plot=plot3, width = 13, height = 8)
ggsave("./jpg/Fig2_map_protect_migrate.jpg", plot=plot3, width = 13, height = 8, dpi = 600)



