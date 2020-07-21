library(tidyverse)
library(dplyr)
library(grid)

nullvals <- rnorm(1000, 10, 2)
nulldens <- dnorm(nullvals, 10, 2)

nulldist <- data.frame(vals = nullvals, dens = nulldens)

altlessvals <- rnorm(1000, 2, 2)
altlessdens <- dnorm(altlessvals, 2, 2)
altlessdist <- data.frame(vals = altlessvals, dens = altlessdens)

altmorevals <- rnorm(1000, 8, 2)
altmoredens <- dnorm(altmorevals, 8, 2)
altmoredist <- data.frame(vals = altmorevals, dens = altmoredens)

plotdata <- data.frame(nullvals = nulldist$vals, nulldens = nulldist$dens, altlessvals = altlessdist$vals, altlessdens = altlessdist$dens, altmorevals = altmoredist$vals, altmoredens = altmoredist$dens)


theme_bluewhite <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightcyan2"),
      panel.border = element_rect(color = "lightblue", fill = NA),
      axis.line = element_line(color = "lightblue"),
      axis.ticks = element_line(color = "lightblue"),
      axis.text = element_text(color = "steelblue")
    )
}

funcShaded <- function(x){
  y <- dnorm(x, mean = 5, sd = 2)
  y[x < 6.5] <- NA
  return(y)
}

funcShaded2 <- function(x){
  y <- dnorm(x, mean = 5, sd = 2)
  y[x < 7] <- NA
  return(y)
}
grob <- grobTree(textGrob("With a two-sided test, we are \n hypothesizing that the alternative \n distribution could be EITHER \n above OR below the null distribution, \n not both -- these are just two \n possibilities.", x=0.05,  y=0.90, hjust=0,
                          gp=gpar(col="darkred", fontsize=9, fontface="italic")))

ggplot(data = plotdata, aes(x = nullvals, y = nulldens)) +
  geom_line(col = "blue", size = 1.5) +
  labs(title = "Plotting our Test Statistic, P-Value, Critical Value, and Alpha", x = "", y = "") +
  scale_x_discrete(limits = c(5)) +
  theme_bluewhite() +
  theme(axis.text.y = element_blank()) +
  geom_vline(xintercept = 7, color = "blueviolet") +
  geom_vline(xintercept = 6.5, color = "palevioletred") +
  geom_area(data = subset(plotdata, nullvals > 7), colour = "blue", fill = "blue",  alpha = 0.4)+
  geom_area(data = subset(plotdata, nullvals > 6.5), colour = "red", fill = "red", alpha = 0.4)+
  geom_text(aes(x=7, label="test statistic", y=0.2), colour="blueviolet", angle=90, vjust = 1, text=element_text(size=11))+
  geom_text(aes(x=6.5, label="critical value", y=0.2), colour="palevioletred", angle=90, vjust = -1, text=element_text(size=11)) +
  annotation_custom(grob)

geom_ribbon(aes(ymin = min(size), ymax = max(size), fill = 'dfg', color = 'dfg'), alpha = 0.2) + 
  geom_area(aes(fill = 'abc', color = 'abc'), alpha = 0.2)
            
scale_color_manual(name="Distribution",
                     breaks = c("Null Distribution", "Alternative Distribution"),
                     values = c("Null Distribution" = "blue", "Alternative Distribution" = "red")) +
  scale_linetype_manual(breaks = c("Null Distribution", "Alternative Distribution"),
                        values = c("Null Distribution" = "solid", "Alternative Distribution" = "dashed"))



altmean <- sample(c(7, 9), 2)
altvals <- rnorm(20, altmean, 2)
sampmean <- mean(altvals)
sampsd <- sd(altvals)

tstat <- (sampmean - 8) / (sampsd / sqrt(20))
if(tstat < 0){
pval <- pt(tstat, 19)
} else{
  pval <- pt(tstat, 19, lower.tail = FALSE)
}
critval1 <- qt(0.05/2, 19)
critval2 <- qt(0.05/2, 19, lower.tail = FALSE)
if(tstat < 0){
  tpos <- qnorm(pval, 8, 1)
} else{
tpos <- qnorm(pval, 8, 1, lower.tail = FALSE)
}
critpos1 <- qnorm(0.05/2, 8, 1)
critpos2 <- qnorm(0.05/2, 8, 1, lower.tail = FALSE)

tvals <- rt(1000, 19)
tdens <- dt(tvals, 19)

tdist <- data.frame(tvals = tvals, tdens = tdens)

ggplot(data = tdist, aes(x = tvals, y = tdens)) +
  geom_line(col = "black", size = 1.5) +
  labs(title = "T-Distribution: Plotting the Test Statistic, P-Value, Critical Value, and Alpha", x = "", y = "") +
  scale_x_discrete(limits = c(0)) +
  theme_bluewhite() +
  theme(axis.text.y = element_blank()) +
  geom_vline(xintercept = tstat, color = "blueviolet", size = 1.5) +
  geom_vline(xintercept = critval1, color = "palevioletred", size = 1.5) +
  geom_vline(xintercept = critval2, color = "palevioletred", size = 1.5) +
  geom_area(data = subset(tdist, tvals > tstat), colour = "blue", fill = "blue",  alpha = 0.4)+
  geom_area(data = subset(tdist, tvals < critval1), colour = "red", fill = "red", alpha = 0.4)+
  geom_area(data = subset(tdist, tvals > critval2), colour = "red", fill = "red", alpha = 0.4)+
  geom_text(aes(x=tstat, label="test statistic", y=0.25), colour="blueviolet", angle=90, vjust = -1, size = 7)+
  geom_text(aes(x=critval1, label="critical value", y=0.25, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7) +
  geom_text(aes(x=critval2, label="critical value", y=0.25, size = 20), colour="palevioletred", angle=90, vjust = 1, size = 7)

