library(ggplot2)
library(scales)
library(data.table)
library(tidyverse)

source("layout.r")

dataMigr <- read.csv("../results/global_output_cummulative_main_cases.csv")
dataNomigr <- read.csv("../results/global_output_control_cummulative_per_SLR.csv")

dataNomigr <- dataNomigr[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migrationcost_submergence_cummulative","seafloodcost_cummulative","protectioncost_cummulative")]
dataMigr <- dataMigr[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migrationcost_submergence_cummulative","seafloodcost_cummulative","protectioncost_cummulative")]

dataMigr$totalcost_cummulative <- dataMigr$migrationcost_submergence_cummulative + dataMigr$seafloodcost_cummulative + dataMigr$protectioncost_cummulative
dataNomigr$totalcost_cummulative <- dataNomigr$migrationcost_submergence_cummulative + dataNomigr$seafloodcost_cummulative + dataNomigr$protectioncost_cummulative

dataNomigr$migrlevel <- "Migrationlevel   0.0"
data <- rbind(dataMigr,dataNomigr)

DT <- data.table(data)
DT[, totalcost_cummulative_mean := mean(totalcost_cummulative), by = c('slr','migrlevel')]
DT[, totalcost_cummulative_min  := min(totalcost_cummulative),  by = c('slr','migrlevel')]
DT[, totalcost_cummulative_max  := max(totalcost_cummulative),  by = c('slr','migrlevel')]
DT[, migrationcost_submergence_cummulative_mean := mean(migrationcost_submergence_cummulative), by = c('slr','migrlevel')]
DT[, seafloodcost_cummulative_mean := mean(seafloodcost_cummulative), by = c('slr','migrlevel')]
DT[, protectioncost_cummulative_mean := mean(protectioncost_cummulative), by = c('slr','migrlevel')]
data <- data.frame(DT)

data <- data[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","totalcost_cummulative_mean","totalcost_cummulative_min","totalcost_cummulative_max","migrationcost_submergence_cummulative_mean","seafloodcost_cummulative_mean","protectioncost_cummulative_mean")]
data <- data[data$ssp=="SSP1",]
data <- data[data$dr=="Discount rate 0.0",]

data$slr <- factor(data$slr, levels=c("RCP 2.6 low","RCP 2.6 hig","RCP 8.5 low","RCP 8.5 hig","H++"))
levels(data$slr)[levels(data$slr)=="H++"] <- "High end"
levels(data$slr)[levels(data$slr)=="RCP 2.6 hig"] <- "RCP 2.6 high"
levels(data$slr)[levels(data$slr)=="RCP 8.5 hig"] <- "RCP 8.5 high"

data$migrlevel <- factor(data$migrlevel, levels=c("Migrationlevel   0.0","Migrationlevel   1.0","Migrationlevel  10.0"))
levels(data$migrlevel)[levels(data$migrlevel)=="Migrationlevel   0.0"] <- "No retreat"
levels(data$migrlevel)[levels(data$migrlevel)=="Migrationlevel   1.0"] <- "Reactive retreat"
levels(data$migrlevel)[levels(data$migrlevel)=="Migrationlevel  10.0"] <- "Proactive retreat"

dataToPlotErrorbars <- data[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","totalcost_cummulative_mean","totalcost_cummulative_min","totalcost_cummulative_max")]
dataToPlotBars <- data[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","totalcost_cummulative_mean","migrationcost_submergence_cummulative_mean","seafloodcost_cummulative_mean","protectioncost_cummulative_mean")]

dataToPlot <- reshape2::melt(dataToPlotBars, id.vars = c("locationid","time","ssp","pfl","dr","slr","migrlevel"), measure.vars = c("totalcost_cummulative_mean","migrationcost_submergence_cummulative_mean","seafloodcost_cummulative_mean","protectioncost_cummulative_mean"))

dataToPlot$value <- dataToPlot$value / 1000

dataToPlotErrorbars$totalcost_cummulative_mean <- dataToPlotErrorbars$totalcost_cummulative_mean / 1000
dataToPlotErrorbars$totalcost_cummulative_max <- dataToPlotErrorbars$totalcost_cummulative_max / 1000
dataToPlotErrorbars$totalcost_cummulative_min <- dataToPlotErrorbars$totalcost_cummulative_min / 1000

levels(dataToPlot$variable)[levels(dataToPlot$variable)=="migrationcost_submergence_cummulative_mean"] <- "Migration cost"
levels(dataToPlot$variable)[levels(dataToPlot$variable)=="seafloodcost_cummulative_mean"] <- "Flood cost"
levels(dataToPlot$variable)[levels(dataToPlot$variable)=="protectioncost_cummulative_mean"] <- "Protection cost"

dataToPlot$variable <- factor(dataToPlot$variable, levels=c("Flood cost","Migration cost","Protection cost","totalcost_cummulative_mean"))

ggplot() + 
    geom_bar(data = filter(dataToPlot, variable %in% c("totalcost_cummulative_mean")), aes(x=migrlevel, y=value), alpha=0.001, color = "black", stat="identity") + 
    geom_bar(data = filter(dataToPlot, variable %in% c("Migration cost","Flood cost","Protection cost")), aes(x=migrlevel, y=value, fill=variable), stat="identity") + 
    geom_errorbar(data = dataToPlotErrorbars, aes(x=migrlevel,ymin=totalcost_cummulative_min,ymax=totalcost_cummulative_max), width=0.4) +
    ylab("Cummulated 21st century cost [US$ Billion]") +
    scale_fill_manual(values=costPalette) + 
    facet_grid(.~slr) +
    theme_bw(25) + 
    theme(axis.title.y=element_text(size=23)) + 
    theme(axis.text.x=element_text(angle=90,vjust=0.5,size=20), axis.title.x=element_blank()) +
    scale_y_continuous(labels = comma, limits=c(0,50000), expand=c(0,0)) +
    theme(legend.position="bottom",legend.title=element_blank()) 

ggsave("./jpg/Fig3_global_totalcost_barplot.jpg", width = 12, height = 9, dpi = 300)
ggsave("./eps/Fig3_global_totalcost_barplot.eps", width = 12, height = 9, dpi = 600)

