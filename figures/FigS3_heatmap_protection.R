library("ggplot2")

data <- read.csv("../results/global_output_cummulative_main_cases.csv")
data$length_protected_rel <- data$length_protected * 100 / data$coastlength

levels(data$slr)[levels(data$slr)=="H++"] <- "High end"

data$rcp_ssp <- paste(data$slr,data$ssp,sep=' / ')

data$rcp_ssp <- factor(data$rcp_ssp, 
levels = c("RCP 2.6 low / SSP3","RCP 2.6 low / SSP4","RCP 2.6 low / SSP2","RCP 2.6 low / SSP1","RCP 2.6 low / SSP5","RCP 2.6 hig / SSP3","RCP 2.6 hig / SSP4","RCP 2.6 hig / SSP2","RCP 2.6 hig / SSP1","RCP 2.6 hig / SSP5","RCP 8.5 low / SSP3","RCP 8.5 low / SSP4","RCP 8.5 low / SSP2","RCP 8.5 low / SSP1","RCP 8.5 low / SSP5","RCP 8.5 hig / SSP3","RCP 8.5 hig / SSP4","RCP 8.5 hig / SSP2","RCP 8.5 hig / SSP1","RCP 8.5 hig / SSP5","High end / SSP3","High end / SSP4","High end / SSP2","High end / SSP1","High end / SSP5")
)

data <- data[,c("dr","rcp_ssp","length_protected_rel","migrlevel")]

data001 <- data[data$migrlevel=="Migrationlevel   1.0",]
data010 <- data[data$migrlevel=="Migrationlevel  10.0",]
data <- merge(data001,data010,by=c("dr","rcp_ssp"))

data$length_protected_rel <- with(data, pmax(length_protected_rel.x,length_protected_rel.y))

ggplot(data, aes(y=rcp_ssp, x=dr)) + 
    geom_tile(aes(fill = length_protected_rel)) +
    geom_text(aes(label = paste (signif(length_protected_rel.x, 2),signif(length_protected_rel.y, 2),sep="/"))) +
    theme_bw(22) + 
    xlab(NULL) + ylab(NULL) + 
    scale_x_discrete(position = "top", labels = c("Discount rate 0.0%","Discount rate 1.5%","Discount rate 3.0%","Discount rate 4.5%","Discount rate 6.0%")) +
    scale_y_discrete(position = "left", labels = c("RCP 2.6 low / SSP3","RCP 2.6 low / SSP4","RCP 2.6 low / SSP2","RCP 2.6 low / SSP1","RCP 2.6 low / SSP5","RCP 2.6 high / SSP3","RCP 2.6 high / SSP4","RCP 2.6 high / SSP2","RCP 2.6 high / SSP1","RCP 2.6 high / SSP5","RCP 8.5 low / SSP3","RCP 8.5 low / SSP4","RCP 8.5 low / SSP2","RCP 8.5 low / SSP1","RCP 8.5 low / SSP5","RCP 8.5 high / SSP3","RCP 8.5 high / SSP4","RCP 8.5 high / SSP2","RCP 8.5 high / SSP1","RCP 8.5 high / SSP5","High end / SSP3","High end / SSP4","High end / SSP2","High end / SSP1","High end / SSP5")) +
    theme(legend.position="bottom") + 
    theme(axis.text.x=element_text(angle = 90, hjust = 1)) + 
    theme(legend.key.width = unit(2.75, "cm"),legend.text=element_text(size=11),legend.title=element_text(size=15)) +
    scale_fill_gradientn(colours = c("red3", "orange", "lawngreen"), space = "Lab", expand=c(0,0), name = "Protected coastline during 21st century (percentage)") +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = .5, label.position = "bottom"))
ggsave("./jpg/FigS3_heatmap_protection.jpg", width = 8, height = 10, dpi = 600)
ggsave("./eps/FigS3_heatmap_protection.eps", width = 8, height = 10)

