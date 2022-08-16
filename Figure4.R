library(ggplot2)
library(cowplot)

setwd("~/Desktop/NSE BP/Figures/1st revision/Figure 4/")

df <- read.csv("./Figure4.csv", header=T)

##### Factors #####
df$bp <- factor(df$bp, levels = c("Hyperactivity","Conduct problems"))

df$rater <- factor(df$rater, levels = c("Parent", "Child"),
                   labels = c("Parent ratings", "Self ratings"))

df$dev <- factor(df$dev, levels = c("Adulthood"),
                 labels = c("Preschool, childhood and adolescence poly-E predicting adulthood behaviour problems"))

##### A #####
dfA <- df[df$comp == "A",]
dfA$variance_type <- factor(dfA$variance_type, levels = c("Total","a123","a44"),
                           labels = c("Total variance",
                                      "Sum of squared paths a14, a24 and a34",
                                      "Squared path a44"))


A <- ggplot(dfA, aes(x = bp, y = variance, fill = variance_type))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  theme(legend.direction = "vertical",
        legend.position="bottom",
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.text.y = element_text(size=10, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=10, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.title= element_text(size=10),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        plot.title = element_text(size=10, angle = 0),
        strip.background = element_rect(colour="black", fill="white"))+
  xlab("")+
  ylab("")+
  #scale_y_continuous(limits = c(0,0.05), breaks = seq(0,0.05,0.01))+
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f","#34a0a4","#8ecae6"))+
  xlab("")+ ylab("Variance in behaviour problems")+
  facet_grid(~dfA$rater, scales = "free", space = "free")

A


##### C #####
dfC <- df[df$comp == "C",]
dfC$variance_type <- factor(dfC$variance_type, levels = c("Total","c123","c44"),
                            labels = c("Total variance",
                                       "Sum of squared paths c14, c24 and c34",
                                       "Squared path c44"))


C <- ggplot(dfC, aes(x = bp, y = variance, fill = variance_type))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  theme(legend.direction = "vertical",
        legend.position="bottom",
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.text.y = element_text(size=10, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=10, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.title= element_text(size=10),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        plot.title = element_text(size=10, angle = 0),
        strip.background = element_rect(colour="black", fill="white"))+
  xlab("")+
  ylab("")+
  #scale_y_continuous(limits = c(0,0.05), breaks = seq(0,0.05,0.01))+
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f","#34a0a4","#8ecae6"))+
  xlab("")+ ylab("Variance in behaviour problems")+
  facet_grid(~dfC$rater, scales = "free", space = "free")

C


##### E #####
dfE <- df[df$comp == "E",]
dfE$variance_type <- factor(dfE$variance_type, levels = c("Total","e123","e44"),
                            labels = c("Total variance",
                                       "Sum of squared paths e14, e24 and e34",
                                       "Squared path e44"))


E <- ggplot(dfE, aes(x = bp, y = variance, fill = variance_type))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  theme(legend.direction = "vertical",
        legend.position="bottom",
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.text.y = element_text(size=10, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=10, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.title= element_text(size=10),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        plot.title = element_text(size=10, angle = 0),
        strip.background = element_rect(colour="black", fill="white"))+
  xlab("")+
  ylab("")+
  #scale_y_continuous(limits = c(0,0.05), breaks = seq(0,0.05,0.01))+
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f","#34a0a4","#8ecae6"))+
  xlab("")+ ylab("Variance in behaviour problems")+
  facet_grid(~dfE$rater, scales = "free", space = "free")

E

##### Combine #####
Figure4 <- ggdraw() +
  draw_plot(E, x = 0, y = 0, height = 1, width = .33) +
  draw_plot(A, x = .33, y = 0, height = 1, width = .33) +
  draw_plot(C, x = .66, y = 0, height = 1, width = .33)

Figure4


ggsave(Figure4, file="./Figure4.tiff", width=10, height=5)  

##### Get legend #####
dfE <- df[df$comp == "E",]
dfE$variance_type <- factor(dfE$variance_type, levels = c("Total","e123","e44"),
                            labels = c("Total variance in behaviour problems in adulthood",
                                       "Variance in behaviour problems in adulthood cumulatively accounted for by the poly-E composites in preschool, childhood and adolescence",
                                       "Variance in behaviour problems in adulthood independent of the poly-E composites in preschool, childhood and adolescence"))


E <- ggplot(dfE, aes(x = bp, y = variance, fill = variance_type))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  theme(legend.direction = "vertical",
        legend.position="bottom",
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.border = element_rect(linetype = 'solid', colour = 'gray',fill = NA),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid'),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.text.y = element_text(size=10, colour="black", family = "sans", angle = 0), 
        axis.text.x = element_text(size=10, colour="black", family = "sans", angle = 45, hjust = 1),
        #axis.text.x = element_blank(),
        axis.title= element_text(size=10),
        strip.text.x = element_text(size=10, angle = 0),
        strip.text.y = element_text(size=10, angle = 0),
        plot.title = element_text(size=10, angle = 0),
        strip.background = element_rect(colour="black", fill="white"))+
  xlab("")+
  ylab("")+
  #scale_y_continuous(limits = c(0,0.05), breaks = seq(0,0.05,0.01))+
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f","#34a0a4","#8ecae6"))+
  xlab("")+ ylab("Variance in behaviour problems")+
  facet_grid(~dfE$rater, scales = "free", space = "free")

E

