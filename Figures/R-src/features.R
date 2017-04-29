library(ggplot2)
library(gridExtra)


features <- read.csv("features.csv", header=FALSE)

df <- data.frame(features)

names(df)[names(df)==names(df[1])] <- "Regression_coefficient"
names(df)[names(df)==names(df[2])] <- "Descriptor_class"
names(df)[names(df)==names(df[3])] <- "Isoform"
names(df)[names(df)==names(df[4])] <- "Distance"


# Gives positive values a steelblue color while negative values are given firebrick color
df$colour <- ifelse(df$Regression_coefficient < 0, "firebrick1","steelblue")

 
ggplot(df, aes(fill=colour, y=Regression_coefficient, x=Distance)) + 
  geom_bar(colour="black", size=0.2, position="dodge", stat="identity") +
  facet_grid(Descriptor_class ~ Isoform) +
  theme_bw() +
  theme(legend.position="none", 
        legend.box = "horizontal",
        strip.background = element_rect(colour="#BBBBBB"),
        panel.border = element_rect(colour="#BBBBBB"),
        axis.text.x = element_text(angle=45),
        axis.title.x = element_text(face="bold", margin = margin(t=10)),
        axis.title.y = element_text(face="bold", margin = margin(r=15))) +
  geom_hline(yintercept = 0) +
  xlab("Node-node distance (\uc5) for GRIND descriptors of compounds") + 
  ylab("Regression coefficient")

