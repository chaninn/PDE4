library(ggplot2)
library(ggrepel)
library(reshape)

data <- read.csv("PDE_final_dataset.csv")
df<- data.frame(data)

External <- subset(df, definition == "External_compound")

pivot_exp <- cast(External, ZINC_ID ~ receptor, value="pIC50")
pivot_pred <- cast(External, ZINC_ID ~ receptor, value="pIC50_Training")

final_exp <- melt(pivot_exp, id="ZINC_ID")
final_pred <- melt(pivot_pred, id="ZINC_ID")

# Experimental
ggplot(pivot_exp, aes(x=pivot_exp$PDE4B, y=pivot_exp$PDE4D)) + 
  #geom_smooth(method=lm, size=0.5, alpha=0.15) +   # Add linear regression line (by default includes 95% confidence region) 
  geom_point(colour="steelblue", alpha=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        panel.border = element_rect(colour="#BBBBBB"),
        strip.background = element_rect(colour="#BBBBBB"),
        axis.title.x = element_text(face="bold", margin = margin(t=10)),
        axis.title.y = element_text(face="bold", margin = margin(r=15)),
        legend.position="none") +
  geom_text_repel(aes(label=ZINC_ID)) +
  xlab(expression(bold(Experimental~pIC[50]~of~PDE4B))) + 
  ylab(expression(bold(Experimental~pIC[50]~of~PDE4D))) +
  geom_abline(alpha=0.15) +
  xlim(6,10) + ylim(6,10)


# Predicted
ggplot(pivot_pred, aes(x=pivot_pred$PDE4B, y=pivot_pred$PDE4D)) + 
  #geom_smooth(method=lm, size=0.5, alpha=0.15) +   # Add linear regression line (by default includes 95% confidence region) 
  geom_point(colour="firebrick1", alpha=0.8) +
  theme_bw() +
  theme(aspect.ratio=1,
        panel.border = element_rect(colour="#BBBBBB"),
        strip.background = element_rect(colour="#BBBBBB"),
        axis.title.x = element_text(face="bold", margin = margin(t=10)),
        axis.title.y = element_text(face="bold", margin = margin(r=15)),
        legend.position="none") +
  geom_text_repel(aes(label=ZINC_ID)) +
  xlab(expression(bold(Predicted~pIC[50]~of~PDE4B))) + 
  ylab(expression(bold(Predicted~pIC[50]~of~PDE4D))) +
  geom_abline(alpha=0.15) +
  xlim(6,10) + ylim(6,10)
