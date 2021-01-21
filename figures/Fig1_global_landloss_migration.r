library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

source("layout.r")

dataMigr <- read.csv("../results/global_output_cummulative_for_landloss_figure.csv")
dataNomigr <- read.csv("../results/global_output_control_cummulative_per_SLR.csv")

dataNomigr <- dataNomigr[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migration_submergence_cummulative_mean","migration_submergence_cummulative_min","migration_submergence_cummulative_max","landloss_submergence_cummulative_mean","landloss_submergence_cummulative_min","landloss_submergence_cummulative_max")]
dataMigr <- dataMigr[,c("locationid","time","ssp","pfl","dr","slr","migrlevel","migration_submergence_cummulative_mean","migration_submergence_cummulative_min","migration_submergence_cummulative_max","landloss_submergence_cummulative_mean","landloss_submergence_cummulative_min","landloss_submergence_cummulative_max")]

dataNomigr$migrlevel <- "Migrationlevel   0.0"
data <- rbind(dataMigr,dataNomigr)

data <- data[data$time==2100,]
data <- data[data$ssp=="SSP1",]
data <- data[data$dr=="Discount rate 0.0",]

data$slr <- factor(data$slr, levels=c("RCP 2.6 low","RCP 2.6 hig","RCP 8.5 low","RCP 8.5 hig","H++"))
levels(data$slr)[levels(data$slr)=="H++"] <- "High end"
levels(data$slr)[levels(data$slr)=="RCP 2.6 hig"] <- "RCP 2.6 high"
levels(data$slr)[levels(data$slr)=="RCP 8.5 hig"] <- "RCP 8.5 high"

data$migration_submergence_cummulative_mean <- data$migration_submergence_cummulative_mean * 6
data$migration_submergence_cummulative_min <- data$migration_submergence_cummulative_min * 6
data$migration_submergence_cummulative_max <- data$migration_submergence_cummulative_max * 6

dataToPlot <- melt(data, id.vars = c("locationid","time","ssp","pfl","dr","slr","migrlevel"), measure.vars = c("migration_submergence_cummulative_mean","migration_submergence_cummulative_min","migration_submergence_cummulative_max","landloss_submergence_cummulative_mean","landloss_submergence_cummulative_min","landloss_submergence_cummulative_max"))

dataToPlotMeans <- dataToPlot[((dataToPlot$variable=="migration_submergence_cummulative_mean") | (dataToPlot$variable=="landloss_submergence_cummulative_mean")),]
names(dataToPlotMeans)[names(dataToPlotMeans) == "value"] <- "value_mean"
levels(dataToPlotMeans$variable)[levels(dataToPlotMeans$variable)=="migration_submergence_cummulative_mean"] <- "Migration"
levels(dataToPlotMeans$variable)[levels(dataToPlotMeans$variable)=="landloss_submergence_cummulative_mean"] <- "Land loss"

dataToPlotMin <- dataToPlot[((dataToPlot$variable=="migration_submergence_cummulative_min") | (dataToPlot$variable=="landloss_submergence_cummulative_min")),]
names(dataToPlotMin)[names(dataToPlotMin) == "value"] <- "value_min"
levels(dataToPlotMin$variable)[levels(dataToPlotMin$variable)=="migration_submergence_cummulative_min"] <- "Migration"
levels(dataToPlotMin$variable)[levels(dataToPlotMin$variable)=="landloss_submergence_cummulative_min"] <- "Land loss"

dataToPlotMax <- dataToPlot[((dataToPlot$variable=="migration_submergence_cummulative_max") | (dataToPlot$variable=="landloss_submergence_cummulative_max")),]
names(dataToPlotMax)[names(dataToPlotMax) == "value"] <- "value_max"
levels(dataToPlotMax$variable)[levels(dataToPlotMax$variable)=="migration_submergence_cummulative_max"] <- "Migration"
levels(dataToPlotMax$variable)[levels(dataToPlotMax$variable)=="landloss_submergence_cummulative_max"] <- "Land loss"

dataToPlot <- merge(dataToPlotMeans, dataToPlotMin, id.vars = c("locationid","time","ssp","pfl","dr","slr","migrlevel","variable"))
dataToPlot <- merge(dataToPlot, dataToPlotMax, id.vars = c("locationid","time","ssp","pfl","dr","slr","migrlevel","variable"))

dataToPlot$migrlevel <- factor(dataToPlot$migrlevel, levels=c("Migrationlevel   0.0","Migrationlevel   1.0","Migrationlevel  10.0"))
levels(dataToPlot$migrlevel)[levels(dataToPlot$migrlevel)=="Migrationlevel   0.0"] <- "No retreat"
levels(dataToPlot$migrlevel)[levels(dataToPlot$migrlevel)=="Migrationlevel   1.0"] <- "Reactive retreat"
levels(dataToPlot$migrlevel)[levels(dataToPlot$migrlevel)=="Migrationlevel  10.0"] <- "Proactive retreat"

dataToPlot$variable <- factor(dataToPlot$variable, levels=c("Land loss","Migration"))

ggplot(dataToPlot, aes(x=migrlevel, y=value_mean,ymin=value_min, ymax=value_max, fill=variable)) + 
    geom_bar(color = "black", stat="identity", position=position_dodge(width=0.9)) + 
    geom_errorbar(position = position_dodge(width=0.9),width=0.5) +
    ylab("Cummulated 21st century land loss [km²]") +
    scale_fill_manual(values=landlossFigurePalette) + 
    facet_grid(.~slr) +
    theme_bw(25) + 
    theme(axis.title.y=element_text(size=20)) + 
    theme(axis.text.x=element_text(angle=90,vjust=0.5,size=19), axis.title.x=element_blank()) +
    scale_y_continuous(labels = comma, limits=c(0,470000), breaks=c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000),expand=c(0,0), sec.axis = sec_axis(~ . / 6000, labels = comma, breaks=c(0,15,30,45,60,75), name = "Cummulated 21st century migration [million people]")) +
    theme(legend.position="bottom",legend.title=element_blank())
ggsave("./jpg/Fig1_global_landloss_migration_cummulative.jpg", width = 12, height = 9, dpi = 600)
ggsave("./eps/Fig1_global_landloss_migration_cummulative.eps", width = 13, height = 10)
