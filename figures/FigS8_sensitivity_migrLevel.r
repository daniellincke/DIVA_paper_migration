library("ggplot2")
library("scales")
library("reshape2")
library(plyr)
library(MASS)

source("layout.r")

d <- read.csv("../results/sensitivity_migrlevel.csv")

d <- d[,c("locationid","time","ssp","dr","slr","landloss_change1_10","landloss_change10_100","migration_change1_10","migration_change10_100")]
d2 <- melt(d, id.vars = c("locationid","time","ssp","dr","slr"), measure.vars = c("landloss_change1_10","landloss_change10_100","migration_change1_10","migration_change10_100"))

d2$var <- ""
d2$action <- ""
d2$index <- 0
d2[d2$variable=="landloss_change1_10",]$var <- "Land loss change [%]"
d2[d2$variable=="landloss_change10_100",]$var <- "Land loss change [%]"
d2[d2$variable=="migration_change1_10",]$var <- "Migration change [%]"
d2[d2$variable=="migration_change10_100",]$var <- "Migration change [%]"
d2[d2$variable=="landloss_change1_10",]$action <- "Retreat level decrease \n[10 year event to 1 year event]"
d2[d2$variable=="landloss_change10_100",]$action <- "Retreat level increase \n[10 year event to 100 year event]"
d2[d2$variable=="migration_change1_10",]$action <- "Retreat level decrease \n[10 year event to 1 year event]"
d2[d2$variable=="migration_change10_100",]$action <- "Retreat level increase \n[10 year event to 100 year event]"

dataFacet1 <- d2[((d2$var == "Land loss change [%]") & (d2$action == "Retreat level decrease \n[10 year event to 1 year event]")),]
dataFacet2 <- d2[(d2$var == "Land loss change [%]") & (d2$action == "Retreat level increase \n[10 year event to 100 year event]"),]
dataFacet3 <- d2[(d2$var == "Migration change [%]") & (d2$action == "Retreat level decrease \n[10 year event to 1 year event]"),]
dataFacet4 <- d2[(d2$var == "Migration change [%]") & (d2$action == "Retreat level increase \n[10 year event to 100 year event]"),]
 
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

dataToPlot$slr <- factor(dataToPlot$slr, levels=c("RCP 2.6 low","RCP 2.6 hig","RCP 8.5 low","RCP 8.5 hig","H++"))

dataToPlot$state <- with(dataToPlot, paste0(var,action))

d1 <- dataToPlot[dataToPlot$state=="Land loss change [%]Retreat level decrease \n[10 year event to 1 year event]",]
d2 <- dataToPlot[dataToPlot$state=="Land loss change [%]Retreat level increase \n[10 year event to 100 year event]",]
d3 <- dataToPlot[dataToPlot$state=="Migration change [%]Retreat level decrease \n[10 year event to 1 year event]",]
d4 <- dataToPlot[dataToPlot$state=="Migration change [%]Retreat level increase \n[10 year event to 100 year event]",]

fit <- fitdistr(d1$value, "normal")
para1 <- fit$estimate
fit <- fitdistr(d2$value, "normal")
para2 <- fit$estimate
fit <- fitdistr(d3$value, "normal")
para3 <- fit$estimate
fit <- fitdistr(d4$value, "normal")
para4 <- fit$estimate

x <- seq(-49,49,0.1)
f1 <- function(x1) { dnorm(x1, para1[1], para1[2]) }
f2 <- function(x2) { dnorm(x2, para2[1], para2[2]) }
f3 <- function(x3) { dnorm(x3, para3[1], para3[2]) }
f4 <- function(x4) { dnorm(x4, para4[1], para4[2]) }

y <- sapply(x, f1)
frame1 <- data.frame(x,y)
frame1$var <- "Land loss change [%]"
frame1$action <- "Retreat level decrease \n[10 year event to 1 year event]"

y <- sapply(x, f2)
frame2 <- data.frame(x,y)
frame2$var <- "Land loss change [%]"
frame2$action <- "Retreat level increase \n[10 year event to 100 year event]"

y <- sapply(x, f3)
frame3 <- data.frame(x,y)
frame3$var <- "Migration change [%]"
frame3$action <- "Retreat level decrease \n[10 year event to 1 year event]"

y <- sapply(x, f4)
frame4 <- data.frame(x,y)
frame4$var <- "Migration change [%]"
frame4$action <- "Retreat level increase \n[10 year event to 100 year event]"

normaldens <- rbind(frame1,frame2,frame3,frame4)
normaldens <- setNames(normaldens, c("value", "density","var","action"))

dataToPlot$state <- with(dataToPlot, paste0(var,action))

d1 <- dataToPlot[dataToPlot$state=="Land loss change [%]Retreat level decrease \n[10 year event to 1 year event]",]
d2 <- dataToPlot[dataToPlot$state=="Land loss change [%]Retreat level increase \n[10 year event to 100 year event]",]
d3 <- dataToPlot[dataToPlot$state=="Migration change [%]Retreat level decrease \n[10 year event to 1 year event]",]
d4 <- dataToPlot[dataToPlot$state=="Migration change [%]Retreat level increase \n[10 year event to 100 year event]",]

fit <- fitdistr(d1$value, "normal")
para1 <- fit$estimate
fit <- fitdistr(d2$value, "normal")
para2 <- fit$estimate
fit <- fitdistr(d3$value, "normal")
para3 <- fit$estimate
fit <- fitdistr(d4$value, "normal")
para4 <- fit$estimate

x <- seq(-36,36,0.1)
f1 <- function(x1) { dnorm(x1, para1[1], para1[2]) }
f2 <- function(x2) { dnorm(x2, para2[1], para2[2]) }
f3 <- function(x3) { dnorm(x3, para3[1], para3[2]) }
f4 <- function(x4) { dnorm(x4, para4[1], para4[2]) }

y <- sapply(x, f1)
frame1 <- data.frame(x,y)
frame1$var <- "Land loss"
frame1$action <- "Retreat level decrease \n[10 year event to 1 year event]"
frame1$me <- para1[1]
frame1$sd <- para1[2]

y <- sapply(x, f2)
frame2 <- data.frame(x,y)
frame2$var <- "Land loss"
frame2$action <- "Retreat level increase \n[10 year event to 100 year event]"
frame2$me <- para2[1]
frame2$sd <- para2[2]

y <- sapply(x, f3)
frame3 <- data.frame(x,y)
frame3$var <- "Migration"
frame3$action <- "Retreat level decrease \n[10 year event to 1 year event]"
frame3$me <- para3[1]
frame3$sd <- para3[2]

y <- sapply(x, f4)
frame4 <- data.frame(x,y)
frame4$var <- "Migration"
frame4$action <- "Retreat level increase \n[10 year event to 100 year event]"
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
    geom_text(aes(x=15, y=0.31,  label=paste("mu",labelme)), data = normaldens, parse=TRUE, hjust=0, vjust=0, size=6) +
    geom_text(aes(x=15, y=0.28, label=paste("sigma",labelsd)), data = normaldens, parse=TRUE, hjust=0, vjust=0, size=6) +
    theme_bw(22) +
    facet_grid(var~action) + 
    scale_x_continuous(label=comma,  limits=c(-36,36), expand=c(0,0), breaks=c(-30,-20,-10,0,10,20,30)) +
    scale_y_continuous(label=comma,  limits=c(0,0.35), expand=c(0,0)) +
    xlab("Change [%]") +
    ylab("frequency / density") +
    theme(plot.margin=unit(c(1,1,1,1),"cm")) + 
    theme(panel.spacing = unit(0.9, "cm"))
ggsave("./eps/FigS8_sensitivity_migrlevel_hist.eps", width = 15, height = 9, dpi = 400)
ggsave("./jpg/FigS8_sensitivity_migrlevel_hist.jpg", width = 15, height = 9, dpi = 400)

