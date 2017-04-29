library(ggplot2)
library(ggrepel)

data <- read.csv("designed-compound_numbers.csv")
df<- data.frame(data)

Type <- as.factor(df$Type)

ggplot(df, aes(x=df$pIC50_for_PDE4B, y=df$pIC50_for_PDE4D, colour=df$Type)) +
  geom_point()+
  theme_bw() +
  theme(aspect.ratio=1,
        panel.border = element_rect(colour="#BBBBBB"),
        strip.background = element_rect(colour="#BBBBBB"),
        axis.title.x = element_text(face="bold", margin = margin(t=10)),
        axis.title.y = element_text(face="bold", margin = margin(r=15)),
        legend.position="none") +
  geom_text_repel(aes(label=ZINC_ID)) +
  xlab(expression(bold(pIC[50]~of~PDE4B))) + 
  ylab(expression(bold(pIC[50]~of~PDE4D))) +
  geom_abline(alpha=0.15)
  #xlim(6,10) + ylim(6,10)
  
  
  
  
  