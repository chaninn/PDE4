library(ggplot2)

data <- read.csv("PDE_results.csv")
df<- data.frame(data)

df$definition <- factor(df$definition,
                       levels = c("Training", "Cross-validation", "External-compound", "External-complex"))

ggplot(df, aes(x=df$pIC50, y=df$pIC50_pred, color=definition)) + 
  geom_point(alpha=0.3) +
  geom_smooth(method=lm, size=0.5, alpha=0.15) +   # Add linear regression line (by default includes 95% confidence region) 
  theme_bw() +
  theme(aspect.ratio=1,
        panel.border = element_rect(colour="#BBBBBB"),
        strip.background = element_rect(colour="#BBBBBB"),
        text = element_text(size=9),
        axis.title.x = element_text(face="bold", margin = margin(t=10)),
        axis.title.y = element_text(face="bold", margin = margin(r=8)),
        legend.position="none") +
  xlab(expression(bold(Experimental~pIC[50]))) + 
  ylab(expression(bold(Predicted~pIC[50]))) +             
  facet_grid(. ~ definition)

