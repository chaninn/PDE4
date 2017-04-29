setwd(getwd())

library(ggplot2)

df <- data.frame(read.csv("leverage_rawData.csv"))

df$definition <- factor(df$definition,
                        levels = c("Training", "External-compound", "External-complex"))

ggplot(df, aes(x=df$Leverage, y=df$Y_Stdnt_Residual, colour=definition)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  theme(aspect.ratio=1,
        panel.border = element_rect(colour="#BBBBBB"),
        strip.background = element_rect(colour="#BBBBBB"),
        axis.title.x = element_text(face="bold", margin = margin(t=10)),
        axis.title.y = element_text(face="bold", margin = margin(r=15)),
        legend.position="none") +
  xlab(expression(bold(Leverage))) + 
  ylab(expression(bold(Y~standardized~residual))) +
  geom_hline(yintercept = 3, color = "red", linetype = 2, alpha=0.5) +
  geom_hline(yintercept = -3, color = "red", linetype = 2, alpha=0.5) +
  geom_vline(xintercept = 0.16, linetype = 2, color = "red", alpha=0.5)
  #xlim(-0.2,1) + ylim(-0.02,1)
#facet_grid(. ~ definition)