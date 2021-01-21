library(ggplot2)

legend <- theme(legend.position="bottom", 
    legend.direction = "horizontal",
    legend.text=element_text(size=16),
    legend.title=element_text(size=16)
    ) 

axis <- theme(axis.text.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.ticks.length = unit(0,"null"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
    )

rotate_axis <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
rotate_axis_18 <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=10))

panel <- theme(
    panel.spacing = unit(0.1,"null")
)

slrPalette  <- c(rgb(0/255, 0/255, 255/255), rgb(0/255, 128/255, 255/255), rgb(255/255, 128/255, 0/255), rgb(255/255, 0/255, 0/255), rgb(255/255, 0/255, 255/255))
slrPalette_reduced  <- c(rgb(0/255, 0/255, 255/255), rgb(255/255, 128/255, 0/255), rgb(255/255, 0/255, 255/255))

costPalette  <- c(rgb(225/255, 0/255, 0/255), rgb(0/255, 0/255, 225/255), rgb(0/255, 225/255, 0/255), rgb(255/255, 255/255, 255/255))
landlossFigurePalette <- c(rgb(50/255, 150/255, 235/255), rgb(225/255, 0/255, 0/255))

sspPalette  <- c("SSP1"=rgb(0/255, 255/255, 0/255), "SSP2"=rgb(255/255, 255/255, 0/255), "SSP3"=rgb(255/255, 0/255, 0/255), "SSP4"=rgb(170/255, 0/255, 85/255), "SSP5"=rgb(0/255, 0/255, 255/255))

dodge <- position_dodge(width=0.9)
blackColor <- c(rgb(0,0,0))
