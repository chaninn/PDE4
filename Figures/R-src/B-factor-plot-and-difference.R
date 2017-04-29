library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)

# B-factor

df <- data.frame(read.csv("B-factor.csv", header=FALSE))

names(df)[names(df)==names(df[1])] <- "Residue_number"
names(df)[names(df)==names(df[2])] <- "B_factor"
names(df)[names(df)==names(df[3])] <- "label"

# 153-171, 196-213, 220-233, 267-279, 311-340, 355-381.

p1 <- ggplot(df, aes(x = Residue_number, y = B_factor, group = label)) +
  geom_rect(aes(xmin=153, xmax=171, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=196, xmax=213, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=220, xmax=233, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=267, xmax=279, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=311, xmax=340, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=355, xmax=381, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_line(aes(colour = factor(label))) +
  #xlab(expression(bold(Residue~number))) +
  ylab(expression(bold(B-factor))) +
  theme_bw() +
  scale_x_continuous(limits=c(80, 420),breaks=seq(100, 400, 50)) +
  scale_y_continuous(limits=c(0.025, 0.150),breaks=seq(0.025, 0.150, 0.025)) +
  theme(panel.border = element_rect(colour="#BBBBBB"),
        strip.background = element_rect(colour="#BBBBBB"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=10),
        #axis.title.x = element_text(face="bold", margin = margin(t=10)),
        axis.title.y = element_text(face="bold", margin = margin(r=15)),
        legend.position="none")

######################################


# B-factor difference

df <- data.frame(read.csv("Bfactor-difference.csv", header=FALSE))

names(df)[names(df)==names(df[1])] <- "Residue_number"
names(df)[names(df)==names(df[2])] <- "B_factor_difference"

# 153-171, 196-213, 220-233, 267-279, 311-340, 355-381.

p2 <- ggplot(df, aes(x = Residue_number, y = B_factor_difference)) +
  geom_rect(aes(xmin=153, xmax=171, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=196, xmax=213, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=220, xmax=233, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=267, xmax=279, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=311, xmax=340, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_rect(aes(xmin=355, xmax=381, ymin=-Inf, ymax=Inf), fill = "lightgrey", alpha = 0.3) +
  geom_bar(stat="identity", position = "identity", width = 1,
           fill=ifelse(df$B_factor_difference > 0,
                       rgb(56,146,208, maxColorValue = 255),
                       rgb(227,111,30, maxColorValue=255))) +
  xlab(expression(bold(Residue~number))) +
  ylab(expression(bold(B-factor~difference))) +
  theme_bw() +
  scale_x_continuous(limits=c(80, 420),breaks=seq(100, 400, 50)) +
  scale_y_continuous(limits=c(-0.050, 0.050),breaks=seq(-0.050, 0.050, 0.025)) +
  theme(panel.border = element_rect(colour="#BBBBBB"),
        strip.background = element_rect(colour="#BBBBBB"),
        text = element_text(size=10),
        axis.title.x = element_text(face="bold", margin = margin(t=45)),
        axis.title.y = element_text(face="bold", margin = margin(r=15)),
        legend.position="bottom")

######################################

# Get the gtables
gA <- ggplotGrob(p1)
gB <- ggplotGrob(p2)

# Set the widths
gA$widths <- gB$widths

gA$heights <- gA$heights * 0.5

gB$heights <- gA$heights

# Arrange the two charts.
# The legend boxes are centered
grid.newpage()
grid.arrange(gA, gB, nrow = 3)
