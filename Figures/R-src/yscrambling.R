setwd("~/")

library(dplyr)
library(ggplot2)

df <- data.frame(read.csv("yscrambling_data.csv"))

df$definition <- factor(df$definition,
                        levels = c("q2", "r2"))

r2_coeff <- coef(lm(df$r2[1:100] ~ df$corr[1:100], data = df))
q2_coeff <- coef(lm(df$r2[101:200] ~ df$corr[101:200], data = df))


ggplot(df, aes(x=df$corr, y=df$r2, colour=definition)) +
  geom_point(alpha=0.5) +
  geom_abline(intercept = r2_coeff[1], slope = r2_coeff[2], 
              alpha=0.8, linetype="dashed", colour="#00BFC4") +
  geom_abline(intercept = q2_coeff[1], slope = q2_coeff[2], 
              alpha=0.8, linetype="dashed", colour="#F8766D") +
  annotate(geom="text", x=0.7, y=0.15, label=paste("list(italic(R^{2}),", " Y~int==0.1072)"),
           #color="#00BFC4", parse=TRUE) +
           parse=TRUE) +
  annotate(geom="text", x=0.7, y=0, label=paste("list(italic(Q^{2}),", " Y~int==0.01300)"),
           #color="#F8766D", parse=TRUE) +
           parse=TRUE) +
  theme_bw() +
  theme(aspect.ratio=1,
        panel.border = element_rect(colour="#BBBBBB"),
        strip.background = element_rect(colour="#BBBBBB"),
        axis.title.x = element_text(face="bold", margin = margin(t=10)),
        axis.title.y = element_text(face="bold", margin = margin(r=15)),
        legend.position="none") +
  xlab(expression(bold(Correlation~coefficient))) + 
  ylab(expression(bold(R^2/Q^2))) +
  xlim(-0.2,1) + ylim(-0.02,1)
  facet_grid(. ~ definition)