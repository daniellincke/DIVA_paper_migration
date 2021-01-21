library("ggplot2")
library("scales")
library("reshape2")
library(plyr)
library(MASS)

source("layout.r")

d <- read.csv("../results/sensitivity_migrcost.csv")
d <- d[,c("locationid","time","ssp","dr","slr","landloss_change3_2","landloss_change3_4","migration_change3_2","migration_change3_4")]

d2 <- melt(d, id.vars = c("locationid","time","ssp","dr","slr"), measure.vars = c("landloss_change3_2","landloss_change3_4","migration_change3_2","migration_change3_4"))

d2$var <- ""
d2$action <- ""
d2$index <- 0
d2[d2$variable=="landloss_change3_2",]$var <- "Land loss change [%]"
d2[d2$variable=="landloss_change3_4",]$var <- "Land loss change [%]"
d2[d2$variable=="migration_change3_2",]$var <- "Migration change [%]"
d2[d2$variable=="migration_change3_4",]$var <- "Migration change [%]"
d2[d2$variable=="landloss_change3_2",]$action <- "Migration cost (per person) decrease \n[3 times GDPC to 2 time GDPC]"
d2[d2$variable=="landloss_change3_4",]$action <- "Migration cost (per person) increase \n[3 times GDPC to 4 time GDPC]"
d2[d2$variable=="migration_change3_2",]$action <- "Migration cost (per person) decrease \n[3 times GDPC to 2 time GDPC]"
d2[d2$variable=="migration_change3_4",]$action <- "Migration cost (per person) increase \n[3 times GDPC to 4 time GDPC]"

dataFacet1 <- d2[((d2$var == "Land loss change [%]") & (d2$action == "Migration cost (per person) decrease \n[3 times GDPC to 2 time GDPC]")),]
dataFacet2 <- d2[(d2$var == "Land loss change [%]") & (d2$action == "Migration cost (per person) increase \n[3 times GDPC to 4 time GDPC]"),]
dataFacet3 <- d2[(d2$var == "Migration change [%]") & (d2$action == "Migration cost (per person) decrease \n[3 times GDPC to 2 time GDPC]"),]
dataFacet4 <- d2[(d2$var == "Migration change [%]") & (d2$action == "Migration cost (per person) increase \n[3 times GDPC to 4 time GDPC]"),]
 
dataFacet1 <- dataFacet1[order(dataFacet1$value),]
dataFacet2 <- dataFacet2[order(dataFacet2$value),]
dataFacet3 <- dataFacet3[order(dataFacet3$value),]
dataFacet4 <- dataFacet4[order(dataFacet4$value),]

dataFacet1$index <- 1:nrow(dataFacet1)
dataFacet2$index <- 1:nrow(dataFacet2)
dataFacet3$index <- 1:nrow(dataFacet3)
dataFacet4$index <- 1:nrow(dataFacet4)

dataToPlot <- rbind(dataFacet1,dataFacet2)
dataToPlot <- rbind(dataToPlot,dataFacet3)
dataToPlot <- rbind(dataToPlot,dataFacet4)

dataToPlot$state <- with(dataToPlot, paste0(var,action))

d1 <- dataToPlot[dataToPlot$state=="Land loss change [%]Migration cost (per person) decrease \n[3 times GDPC to 2 time GDPC]",]
d2 <- dataToPlot[dataToPlot$state=="Land loss change [%]Migration cost (per person) increase \n[3 times GDPC to 4 time GDPC]",]
d3 <- dataToPlot[dataToPlot$state=="Migration change [%]Migration cost (per person) decrease \n[3 times GDPC to 2 time GDPC]",]
d4 <- dataToPlot[dataToPlot$state=="Migration change [%]Migration cost (per person) increase \n[3 times GDPC to 4 time GDPC]",]

#print(quantile(d1$value, c(.05, .95)))

fit <- fitdistr(d1$value, "normal")
para1 <- fit$estimate
fit <- fitdistr(d2$value, "normal")
para2 <- fit$estimate
fit <- fitdistr(d3$value, "normal")
para3 <- fit$estimate
fit <- fitdistr(d4$value, "normal")
para4 <- fit$estimate

x <- seq(-29,29,0.1)
f1 <- function(x1) { dnorm(x1, para1[1], para1[2]) }
f2 <- function(x2) { dnorm(x2, para2[1], para2[2]) }
f3 <- function(x3) { dnorm(x3, para3[1], para3[2]) }
f4 <- function(x4) { dnorm(x4, para4[1], para4[2]) }

y <- sapply(x, f1)
frame1 <- data.frame(x,y)
frame1$var <- "Land loss"
frame1$action <- "Migration cost (per person) decrease \n[3 times GDPC to 2 time GDPC]"
frame1$me <- para1[1]
frame1$sd <- para1[2]

y <- sapply(x, f2)
frame2 <- data.frame(x,y)
frame2$var <- "Land loss"
frame2$action <- "Migration cost (per person) increase \n[3 times GDPC to 4 time GDPC]"
frame2$me <- para2[1]
frame2$sd <- para2[2]

y <- sapply(x, f3)
frame3 <- data.frame(x,y)
frame3$var <- "Migration"
frame3$action <- "Migration cost (per person) decrease \n[3 times GDPC to 2 time GDPC]"
frame3$me <- para3[1]
frame3$sd <- para3[2]

y <- sapply(x, f4)
frame4 <- data.frame(x,y)
frame4$var <- "Migration"
frame4$action <- "Migration cost (per person) increase \n[3 times GDPC to 4 time GDPC]"
frame4$me <- para4[1]
frame4$sd <- para4[2]

normaldens <- rbind(frame1,frame2,frame3,frame4)
normaldens <- setNames(normaldens, c("value", "density","var","action","me","sd"))

normaldens$labelme <- with(normaldens, paste0('==', formatC(round(me, 3), format='f', digits=3)))
normaldens$labelsd <- with(normaldens, paste0('==', formatC(round(sd, 3), format='f', digits=3)))

dataToPlot[dataToPlot$var=="Land loss change [%]",]$var <- "Land loss"
dataToPlot[dataToPlot$var=="Migration change [%]",]$var <- "Migration"

ggplot(dataToPlot, aes(x=value)) + 
    geom_histogram(aes(y = ..density..), position = "identity", binwidth=2) +
    geom_line(aes(y = density), data = normaldens, colour = "red", size=1.4) +
    geom_text(aes(x=15, y=0.6,  label=paste("mu",labelme)), data = normaldens, parse=TRUE, hjust=0, vjust=0, size=6) +
    geom_text(aes(x=15, y=0.54, label=paste("sigma",labelsd)), data = normaldens, parse=TRUE, hjust=0, vjust=0, size=6) +
    theme_bw(22) +
    facet_grid(var~action) + 
    scale_x_continuous(label=comma,  limits=c(-30,30), expand=c(0,0)) +
    scale_y_continuous(label=comma,  limits=c(0,0.68), expand=c(0,0)) +
    xlab("Change [%]") +
    ylab("frequency / density") +
    theme(plot.margin=unit(c(1,1,1,1),"cm")) + 
    theme(panel.spacing = unit(0.9, "cm"))
ggsave("./eps/FigS7_sensitivity_migrcost_hist.eps", width = 15, height = 9)
ggsave("./jpg/FigS7_sensitivity_migrcost_hist.jpg", width = 15, height = 9, dpi = 600)

