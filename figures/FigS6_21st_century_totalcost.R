library(ggplot2)
library(scales)
library(data.table)

source("layout.r")

rcpSubstring <- function(s) {
    if(length(grep("rcp26",s))>0) "RCP 2.6"
    else if(length(grep("rcp85",s))>0) "RCP 8.5"
    else "H++"
}

iceSubstring <- function(s) {
    if(length(grep("low",s))>0) "low"
    else "hig"
}

data <- read.csv("../results/global_output.csv")

data <- data[data$time>2010,]

data$rcp <- sapply(data$caseid, rcpSubstring)
data$rcp <- as.factor(data$rcp)

data$ice <- sapply(data$caseid, iceSubstring)
data$ice <- as.factor(data$ice)


data <- data[((data$migrlevel=="Migrationlevel   1.0") | (data$migrlevel=="Migrationlevel  10.0")),]
data <- data[data$migrcost=="Migrationcost 3.0",]

data <- data[,c("time","seadike_cost","seadike_maintenance_cost","seafloodcost","migrationcost_submergence","ssp","dr","rcp","ice","migrlevel")]

data$seadike_cost <- data$seadike_cost / 1000
data$seafloodcost <- data$seafloodcost / 1000
data$migrationcost_submergence <- data$migrationcost_submergence / 1000
data$seadike_maintenance_cost <- data$seadike_maintenance_cost / 1000

data$totalcost <- data$seafloodcost + data$seadike_cost + data$seadike_maintenance_cost + data$migrationcost_submergence
data$protection_cost <- data$seadike_cost + data$seadike_maintenance_cost

DT <- data.table(data)
DT[, totalcost_min := min(totalcost), by = c('time','rcp','ssp')]
DT[, totalcost_max := max(totalcost), by = c('time','rcp','ssp')]
DT[, totalcost_avg := mean(totalcost), by = c('time','rcp','ssp')]
DT[, protection_cost_min := min(protection_cost), by = c('time','rcp','ssp')]
DT[, protection_cost_max := max(protection_cost), by = c('time','rcp','ssp')]
DT[, protection_cost_avg := mean(protection_cost), by = c('time','rcp','ssp')]
DT[, seafloodcost_min := min(seafloodcost), by = c('time','rcp','ssp')]
DT[, seafloodcost_max := max(seafloodcost), by = c('time','rcp','ssp')]
DT[, seafloodcost_avg := mean(seafloodcost), by = c('time','rcp','ssp')]
DT[, migrationcost_submergence_min := min(migrationcost_submergence), by = c('time','rcp','ssp')]
DT[, migrationcost_submergence_max := max(migrationcost_submergence), by = c('time','rcp','ssp')]
DT[, migrationcost_submergence_avg := mean(migrationcost_submergence), by = c('time','rcp','ssp')]
data <- data.frame(DT)

data <- data[data$dr=="Discount rate 0.0",]
data <- data[data$migrlevel=="Migrationlevel   1.0",]
data <- data[data$ice=="hig",]
data <- transform(data, rcp=factor(rcp,levels=c("RCP 2.6","RCP 8.5","H++")))
levels(data$rcp)[levels(data$rcp)=="H++"] <- "High end"

ggplot(data) +
    geom_path(aes(x=time, y=seafloodcost_avg, color=rcp, group=rcp), size=0.5, linetype="solid") + 
    geom_ribbon(aes(x=time, ymin=seafloodcost_min, ymax=seafloodcost_max, color=rcp, fill=rcp), linetype=0, alpha=0.3) + 
    theme_bw(20) + 
    xlab("Year") + ylab("Damage cost [billion US$/yr]") + 
    scale_x_continuous(breaks=c(2015,2025,2050,2075,2100),expand=c(0,0)) + 
    scale_y_continuous(label=comma,expand=c(0,0),limits=c(0,400)) + 
    facet_wrap(~ssp, ncol=5) + 
    legend + 
    rotate_axis +
    panel +
    scale_colour_manual(values=slrPalette_reduced, guide = guide_legend(title = "SLR scenario")) + 
    scale_fill_manual(values=slrPalette_reduced, guide = guide_legend(title = "SLR scenario")) 
ggsave("./eps/FigS6_21st_century_seafloodcost.eps", width = 14, height = 5)
ggsave("./jpg/FigS6_21st_century_seafloodcost.jpg", width = 14, height = 5, dpi = 600)

ggplot(data) +
    geom_path(aes(x=time, y=protection_cost_avg, color=rcp, group=rcp), size=0.5, linetype="solid") + 
    geom_ribbon(aes(x=time, ymin=protection_cost_min, ymax=protection_cost_max, color=rcp, fill=rcp), linetype=0, alpha=0.3) + 
    theme_bw(20) + 
    xlab("Year") + ylab("Protection cost [billion US$/yr]") + 
    scale_x_continuous(breaks=c(2015,2025,2050,2075,2100),expand=c(0,0)) + 
    scale_y_continuous(label=comma,expand=c(0,0),limits=c(0,400)) + 
    facet_wrap(~ssp, ncol=5) + 
    legend + 
    rotate_axis +
    panel +
    scale_colour_manual(values=slrPalette_reduced, guide = guide_legend(title = "SLR scenario")) + 
    scale_fill_manual(values=slrPalette_reduced, guide = guide_legend(title = "SLR scenario")) 
ggsave("./eps/FigS6_21st_century_protectioncost.eps", width = 14, height = 5)
ggsave("./jpg/FigS6_21st_century_protectioncost.jpg", width = 14, height = 5, dpi = 600)

ggplot(data) +
    geom_path(aes(x=time, y=migrationcost_submergence_avg, color=rcp, group=rcp), size=0.5, linetype="solid") + 
    geom_ribbon(aes(x=time, ymin=migrationcost_submergence_min, ymax=migrationcost_submergence_max, color=rcp, fill=rcp), linetype=0, alpha=0.3) + 
    theme_bw(20) + 
    xlab("Year") + ylab("Migration cost [billion US$/yr]") + 
    scale_x_continuous(breaks=c(2015,2025,2050,2075,2100),expand=c(0,0)) + 
    scale_y_continuous(label=comma,expand=c(0,0),limits=c(0,400)) + 
    facet_wrap(~ssp, ncol=5) + 
    legend + 
    rotate_axis +
    panel +
    scale_colour_manual(values=slrPalette_reduced, guide = guide_legend(title = "SLR scenario")) + 
    scale_fill_manual(values=slrPalette_reduced, guide = guide_legend(title = "SLR scenario")) 
ggsave("./eps/FigS6_21st_century_migrationcost.eps", width = 14, height = 5)
ggsave("./jpg/FigS6_21st_century_migrationcost.jpg", width = 14, height = 5, dpi = 600)

ggplot(data) +
    geom_path(aes(x=time, y=totalcost_avg, color=rcp, group=rcp), size=0.5, linetype="solid") + 
    geom_ribbon(aes(x=time, ymin=totalcost_min, ymax=totalcost_max, color=rcp, fill=rcp), linetype=0, alpha=0.3) + 
    theme_bw(20) + 
    xlab("Year") + ylab("SLR cost [billion US$/yr]") + 
    scale_x_continuous(breaks=c(2015,2025,2050,2075,2100),expand=c(0,0)) + 
    scale_y_continuous(label=comma,expand=c(0,0),limits=c(0,500)) + 
    facet_wrap(~ssp, ncol=5) + 
    legend + 
    rotate_axis +
    panel +
    scale_colour_manual(values=slrPalette_reduced, guide = guide_legend(title = "SLR scenario")) + 
    scale_fill_manual(values=slrPalette_reduced, guide = guide_legend(title = "SLR scenario")) 
ggsave("./eps/FigS6_21st_century_totalcost.eps", width = 14, height = 5)
ggsave("./jpg/FigS6_21st_century_totalcost.jpg", width = 14, height = 5, dpi = 600)

