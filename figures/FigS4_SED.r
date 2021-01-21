library(ggplot2)
library(scales)

data <- read.csv("../results/global_output.csv")

source("layout.r")

# just extract one case

data <- data[data$slr=="H++",]
data <- data[data$migr=="Migration",]
data <- data[data$pfl=="Table protection levels",]
data <- data[data$dr=="Discount rate 0.0",]
data <- data[data$seadikecost_factor=="Usual seadike cost",]

data$ssp <- factor(data$ssp, levels=c("SSP1","SSP2","SSP3","SSP4","SSP5"))

data$gdp <- data$gdp / 1000000

ggplot(data,aes(x=time, y=gdp, colour=ssp)) + 
    geom_line(size=1.5) +
    xlab("Year") + 
    ylab("Global GDP [US$ trillion]") +
    theme_bw(24) +
    scale_y_continuous(labels = comma) + 
    scale_x_continuous(breaks=c(2000, 2025, 2050, 2075, 2100), limits = c(2000, 2100), expand = c(0,0)) +
    scale_colour_manual(values=sspPalette) +
    legend + 
    theme(axis.text.x=element_text(angle=90,vjust=0.5)) + 
    guides(color=guide_legend(title=NULL,nrow=1,byrow=TRUE))
ggsave("./jpg/FigS4_global_gdp.jpg", width = 9, height = 6, dpi = 600)
ggsave("./eps/FigS4_global_gdp.eps", width = 9, height = 6)

data$totalpop <- data$totalpop / 1000000

ggplot(data,aes(x=time, y=totalpop, colour=ssp)) + 
    geom_line(size=1.5) +
    xlab("Year") + 
    ylab("Global population [billion]") +
    theme_bw(24) +
    scale_y_continuous(labels = comma) + 
    scale_x_continuous(breaks=c(2000, 2025, 2050, 2075, 2100), limits = c(2000, 2100), expand = c(0,0)) +
    scale_colour_manual(values=sspPalette) +
    legend + 
    theme(axis.text.x=element_text(angle=90,vjust=0.5)) + 
    guides(color=guide_legend(title=NULL,nrow=1,byrow=TRUE))
ggsave("./jpg/FigS4_global_pop.jpg", width = 9, height = 6, dpi = 600)
ggsave("./eps/FigS4_global_pop.eps", width = 9, height = 6)

