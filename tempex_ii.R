#!/usr/bin/env RScript

library(data.table)
library(Cairo)

sun <- '\u2600'
snowflake <- '\u2744'
auto <- '\u21c4'
degree <- '\u00B0'
blue <- '#2e7290'

format_treatment_text <- function(text) {
    text <- gsub('off', 'Off', text)
    text <- gsub('_star', '\\*', text)
    text <- gsub('a', auto, text)
    text <- gsub('c', snowflake, text)
    text <- gsub('h', sun, text)
    text <- gsub('_', paste0(degree, 'C '), text)

    return(text)
}

IMAGE_WIDTH <- 4
IMAGE_HEIGHT <- 4*150/417
IMAGE_MARGINS_STANDARD <- c(3.6, 4.1, 2.6, 4.1)
IMAGE_MARGINS_AVERAGE <- c(3.6, 4.1, 2.6, 1.1)
CEX <- 0.5

filename <- 'temperature_experiment_ii.csv'
temp <- read.csv(filename)
temp.t <- data.table(temp)

all_means <- temp.t[, list(mean_rating=mean(rating), sd_rating=sd(rating)), by=treatment][order(-mean_rating)]
all_means
#    treatment mean_rating sd_rating
# 1:       off    2.790000 0.2643651
# 2:      24_a    2.700000 0.3651484
# 3: 23_a_star    2.555556 0.3711843
# 4:      24_h    2.362500 0.4565007
# 5:      23_c    2.330000 0.4808557
# 6:      23_h    2.318182 0.6400284
# 7:      23_a    2.262500 0.5244586
# 8:      24_c    2.160000 0.6275172
CairoFonts(regular = 'Arial Unicode MS:style=Regular') #family="Arial Unicode MS"

CairoPNG('TemperatureExperimentII.png', res=900, width=IMAGE_WIDTH, height=IMAGE_HEIGHT, units='in', bg='transparent')
par(mar=IMAGE_MARGINS_AVERAGE, cex=CEX, xpd=NA)
bp <- barplot(height=all_means$mean_rating,
        col=blue,
        border=NA,
        ylim=c(0, 3),
        xlab='',
        ylab='Mean contentedness rating',
        main='Zone 2',
        names.arg=format_treatment_text(all_means$treatment))

# axis(side=1, at=bp, labels=format_treatment_text(all_means$treatment), cex.axis=CEX)
mtext(side=1, line=2, cex=CEX, text="Treatment")

text(x=bp, y=0.25, labels=round(all_means$mean_rating, 2), col="white")

text(cex=2.75*CEX/2,
     x=6.75,
     y=2.75,
     labels=paste0("*For this treatment, Zone 3 was also set to 23", degree, "C ", auto, ", rather than its normal 22", degree, "C ", auto, "."))

dev.off()