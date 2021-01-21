library("ggplot2")
library("scales")

source("layout.r")

data <- read.csv("../results/global_output_cummulative_all_timesteps.csv")

data$slr <- factor(data$slr, levels=c("RCP 2.6 low","RCP 2.6 hig","RCP 8.5 low","RCP 8.5 hig","H++"))
model <- lm(data$landloss_submergence_cummulative ~ data$rslr)
print(coef(model)["data$rslr"])
print(coef(model)["(Intercept)"])
print(summary(model)$r.squared)

ggplot(data, aes(y=landloss_submergence_cummulative, x=rslr, colour=slr)) + 
    geom_point() +
    geom_abline(slope = coef(model)["data$rslr"], intercept = coef(model)["(Intercept)"] ) + 
    geom_text(aes(1.0,380000,label = "land loss = 251,544 * sl change + 2,959.7", vjust = -1), size=7, color="black") + 
    theme_bw(22) +
    scale_y_continuous(labels = comma, limits=c(0,435000), expand=c(0,0)) +
    scale_x_continuous(labels = comma, limits=c(0,2), expand=c(0,0)) +
    scale_colour_manual(values=slrPalette) + 
    xlab("Global mean sea-level change [m]") + ylab("Land loss due to submergence [km²]") + 
    theme(legend.position="bottom",legend.title=element_blank()) + 
    theme(plot.margin=unit(c(1,1,1,1),"cm"))
ggsave("./jpg/FigS1_landloss_curve.jpg", width = 10, height = 10, dpi = 600)
ggsave("./eps/FigS1_landloss_curve.eps", width = 10, height = 10)

