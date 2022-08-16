library(ggplot2)
library(cowplot)

setwd("~/Desktop/NSE BP/Figures/1st revision/Figure 3/")

df <- read.csv("./Figure3.csv", header=T)

##### Factors #####

df$bp <- factor(df$bp, levels = c("Hyperactivity", "Conduct problems",
                                  "Emotional problems", "Peer problems"))

df$rater <- factor(df$rater, levels = c("Parent", "Teacher", "Self"),
                   labels = c("Parent ratings", "Teacher ratings", "Self ratings"))

df$dev <- factor(df$dev, levels = c("Childhood", "Adolescence", "Adulthood"),
                 labels = c("Preschool poly-E predicting childhood behaviour problems",
                            "Childhood poly-E predicting adolescence behaviour problems",
                            "Adolescence poly-E predicting adulthood behaviour problems"))

##### A #####
dfA <- df[df$comp == "A",]
dfA$variance_type <- factor(dfA$variance_type, levels = c("Total","a12","NSE"),
                           labels = c("Total variance",
                                      "Standardized squared path a12 (genetic variance shared between poly-E and behaviour problems)",
                                      "Standardized squared path a22 (genetic variance unique to behaviour problems)"))

##### Childhood #####
df1 <- dfA[dfA$dev == "Preschool poly-E predicting childhood behaviour problems",]

ch = ggplot(df1, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df1$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Preschool poly-E predicting childhood behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

ch

##### Adolescence #####
df2 <- dfA[dfA$dev == "Childhood poly-E predicting adolescence behaviour problems",]

ado = ggplot(df2, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df2$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Childhood poly-E predicting adolescence behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

ado

##### Adulthood #####
df3 <- dfA[dfA$dev == "Adolescence poly-E predicting adulthood behaviour problems",]

adu = ggplot(df3, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df3$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Adolescence poly-E predicting adulthood behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

adu
ggsave(adu, file="./adu.tiff", width=15, height=15)  

##### Grid #####
A <- ggdraw() +
  draw_plot(ch, x = 0, y = .66, height = .33, width = 1) +
  draw_plot(ado, x = 0, y = .33, height = .33, width = 1) +
  draw_plot(adu, x = 0, y = 0, height = .33, width = .60)

A

ggsave(A, file="./A.tiff", width=7, height=15)  

##### C #####
dfC <- df[df$comp == "C",]
dfC$variance_type <- factor(dfC$variance_type, levels = c("Total","c12","NSE"),
                            labels = c("Total variance",
                                       "Standardized squared path c12 (shared environmental variance shared between poly-E and behaviour problems)",
                                       "Standardized squared path c22 (shared environmental variance unique to behaviour problems)"))

##### Childhood #####
df1 <- dfC[dfC$dev == "Preschool poly-E predicting childhood behaviour problems",]

ch = ggplot(df1, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df1$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Preschool poly-E predicting childhood behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

ch

##### Adolescence #####
df2 <- dfC[dfC$dev == "Childhood poly-E predicting adolescence behaviour problems",]

ado = ggplot(df2, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df2$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Childhood poly-E predicting adolescence behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

ado

##### Adulthood #####
df3 <- dfC[dfC$dev == "Adolescence poly-E predicting adulthood behaviour problems",]

adu = ggplot(df3, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df3$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Adolescence poly-E predicting adulthood behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

adu

##### Grid #####
C <- ggdraw() +
  draw_plot(ch, x = 0, y = .66, height = .33, width = 1) +
  draw_plot(ado, x = 0, y = .33, height = .33, width = 1) +
  draw_plot(adu, x = 0, y = 0, height = .33, width = .60)

C

ggsave(C, file="./C.tiff", width=7, height=15)  

##### E #####
dfE <- df[df$comp == "E",]
dfE$variance_type <- factor(dfE$variance_type, levels = c("Total","e12","NSE"),
                            labels = c("Total variance",
                                       "Standardized squared path e12 (NSE variance shared between poly-E and behaviour problems)",
                                       "Standardized squared path e22 (NSE variance unique to behaviour problems)"))

##### Childhood #####
df1 <- dfE[dfE$dev == "Preschool poly-E predicting childhood behaviour problems",]

ch = ggplot(df1, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df1$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Preschool poly-E predicting childhood behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

ch

##### Adolescence #####
df2 <- dfE[dfE$dev == "Childhood poly-E predicting adolescence behaviour problems",]

ado = ggplot(df2, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df2$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Childhood poly-E predicting adolescence behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

ado

##### Adulthood #####
df3 <- dfE[dfE$dev == "Adolescence poly-E predicting adulthood behaviour problems",]

adu = ggplot(df3, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df3$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Adolescence poly-E predicting adulthood behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

adu

##### Grid #####
E <- ggdraw() +
  draw_plot(ch, x = 0, y = .66, height = .33, width = 1) +
  draw_plot(ado, x = 0, y = .33, height = .33, width = 1) +
  draw_plot(adu, x = 0, y = 0, height = .33, width = .60)

E

ggsave(E, file="./E.tiff", width=7, height=15)  

##### Combine #####
Figure3 <- ggdraw() +
  draw_plot(E, x = 0, y = 0, height = 1, width = .33) +
  draw_plot(A, x = .33, y = 0, height = 1, width = .33) +
  draw_plot(C, x = .66, y = 0, height = 1, width = .33)

Figure3


ggsave(Figure3, file="./Figure3.tiff", width=20, height=15)  


##### Get legend #####
dfE <- df[df$comp == "E",]
dfE$variance_type <- factor(dfE$variance_type, levels = c("Total","e12","NSE"),
                            labels = c("Total variance in behaviour problems",
                                       "Variance in behaviour problems accounted for by the poly-E composites",
                                       "Variance in behaviour problems independent of the poly-E composites"))

df1 <- dfE[dfE$dev == "Preschool poly-E predicting childhood behaviour problems",]

ch = ggplot(df1, aes(x = bp, y = variance, fill = variance_type, color="black"))+
  geom_bar(stat = "identity", color = "black", position = "stack", width = 0.9, size = 0.5) +
  facet_grid(~df1$rater, space = "free", scales = "free")+
  theme(legend.position='right',
        legend.direction = 'vertical',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(angle = 0, size = 15),
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        plot.title = element_text(size = 15, colour="black", family = "sans"),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  ggtitle("Preschool poly-E predicting childhood behaviour problems")+
  guides(fill=guide_legend(title=" "))+
  #geom_text(aes(label = NSE_exp), hjust = 0, vjust = 0)+
  #geom_text(aes(Label = Sig), hjust = -25, vjust = .7, colour="black", size = 15) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  scale_color_manual(values = c("black"))+
  scale_y_continuous(limits = c(0, 1))+
  xlab("")+ ylab("Variance in behaviour problems")

ch

