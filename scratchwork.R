library(tidyverse)
library(dplyr)

nullvals <- rnorm(1000, 5, 2)
nulldens <- dnorm(nullvals, 5, 2)
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
  geom_text(aes(x=6.5, label="critical value", y=0.2), colour="palevioletred", angle=90, vjust = -1, text=element_text(size=11)) 

geom_ribbon(aes(ymin = min(size), ymax = max(size), fill = 'dfg', color = 'dfg'), alpha = 0.2) + 
  geom_area(aes(fill = 'abc', color = 'abc'), alpha = 0.2)
            
scale_color_manual(name="Distribution",
                     breaks = c("Null Distribution", "Alternative Distribution"),
                     values = c("Null Distribution" = "blue", "Alternative Distribution" = "red")) +
  scale_linetype_manual(breaks = c("Null Distribution", "Alternative Distribution"),
                        values = c("Null Distribution" = "solid", "Alternative Distribution" = "dashed"))



altmean <- sample(c(7, 9), 1)
altvals <- rnorm(20, altmean, 1)
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
