library(tidyverse)
library(dplyr)

nullvals <- rnorm(1000, 5, 1)
nulldens <- dnorm(nullvals, 5, 1)
nulldist <- data.frame(vals = nullvals, dens = nulldens)

altlessvals <- rnorm(1000, 2, 1)
altlessdens <- dnorm(altlessvals, 2, 1)
altlessdist <- data.frame(vals = altlessvals, dens = altlessdens)

altmorevals <- rnorm(1000, 8, 1)
altmoredens <- dnorm(altmorevals, 8, 1)
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

ggplot(data = plotdata, aes(x = nullvals, y = nulldens)) +
  geom_line(col = "blue", size = 1.5) +
  geom_line(data = plotdata, aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
  geom_line(data = plotdata, aes(x = altmorevals, y = altmoredens), col = "red", linetype ="dashed") +
  labs(title = "Plotting The Null and Alternative Distributions", x = "", y = "") +
  scale_x_discrete(limits = c(5)) +
  theme_bluewhite() +
  theme(axis.text.y = element_blank()) +
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
