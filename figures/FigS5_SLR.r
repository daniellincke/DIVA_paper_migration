library(ggplot2)
library(scales)

data <- read.csv("../results/global_output.csv")

source("layout.r")

# just extract one case

data <- data[data$ssp=="SSP1",]
data <- data[data$dr=="Discount rate 0.0",]
data <- data[data$migrcost=="Migrationcost 2.0",]
data <- data[data$migrlevel=="Migrationlevel   1.0",]

data$slr <- factor(data$slr, levels=c("RCP 2.6 low","RCP 2.6 hig","RCP 8.5 low","RCP 8.5 hig","H++"))
levels(data$slr) <- c("RCP 2.6 (5th percentile)","RCP 2.6 (95th percentile)","RCP 8.5 (5th percentile)","RCP 8.5 (95th percentile)","H++")

ggplot(data,aes(x=time, y=slr_climate_induced, colour=slr)) + 
    geom_line(size=1.5) +
    ggtitle(expression(atop("Global coastal mean sea-level change", atop(italic("all scenarios"), "")))) +
    xlab("Year") + 
    ylab("Sea level change [m]") +
    theme_bw(18) +
    legend + 
    theme(legend.title = element_blank()) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits=c(-0.1, 2.10), breaks=c(0, 0.5, 1.0, 1.5, 2.0), expand = c(0,0)) + 
    scale_x_continuous(breaks=c(2000, 2025, 2050, 2075, 2100), limits = c(2000, 2100), expand = c(0,0)) +
    scale_colour_manual(values=slrPalette) +
    theme(axis.text.x=element_text(angle=90,vjust=0.5)) + 
    guides(color=guide_legend(title="Sea-level rise scenario  ",nrow=3,byrow=TRUE))
ggsave("./jpg/FigS5_slr.jpg", width = 14, height = 5.5, dpi = 600)
ggsave("./eps/FigS5_slr.eps", width = 14, height = 5.5)
