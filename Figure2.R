###
# NSE BP: univ twin results
# Aga 24/06/2022
###

library(ggplot2)
library(ggsci)
library(gridExtra)
library(tidyverse)
library(cowplot)

setwd('~/Desktop/NSE BP/Figures/1st revision/Figure 2/')

df_bp <- read.csv("./univ_BP.csv", header=T)
df_polyE <- read.csv("./univ_polyE.csv", header=T)

##### BP #####
## Factors
df_bp$bp <- factor(df_bp$bp, levels = c("Peer problems", "Emotional problems", "Conduct problems",
                                        "Hyperactivity"))

df_bp$rater <- factor(df_bp$rater, levels = c("Parent", "Teacher", "Self"),
                      labels = c("Parent ratings", "Teacher ratings", "Self ratings"))

df_bp$dev <- factor(df_bp$dev, levels = c("Preschool", "Childhood", "Adolescence", 
                                        "Adulthood"))

df_bp$comp <- factor(df_bp$comp, levels = c("E", "C", "A"),
                     labels = c("NSE influences", "Shared environmental influences", "Genetic influences"))

## Preschool
df_pr <- df_bp[df_bp$dev == "Preschool",]

pr <- ggplot(df_pr, aes(x = bp,
                   y = est, fill=comp))+
  coord_flip()+
  ggtitle("Preschool")+
  geom_bar(stat = "identity", color = "black", position = "stack", width = .9, size = .5)+
  facet_wrap(df_pr$rater)+
  #facet_grid(df$dev, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15,colour="black", family = "sans"),
        
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = Sig), hjust = 0, vjust = 0)+
  #geom_text(aes(label = Sig), hjust = -25, vjust = .7, colour="black", size = 25) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  #scale_y_continuous(limits = c(-4, 22), breaks = seq(-4, 22, 5))+
  #scale_x_discrete(labels = c("Females","Males"))+
  xlab("")+ ylab("Proportion of variance explained")

pr

## Childhood
df_ch <- df_bp[df_bp$dev == "Childhood",]

ch <- ggplot(df_ch, aes(x = bp,
                        y = est, fill=comp))+
  coord_flip()+
  ggtitle("Childhood")+
  geom_bar(stat = "identity", color = "black", position = "stack", width = .9, size = .5)+
  facet_wrap(df_ch$rater)+
  #facet_grid(df$dev, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15,colour="black", family = "sans"),
        
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = Sig), hjust = 0, vjust = 0)+
  #geom_text(aes(label = Sig), hjust = -25, vjust = .7, colour="black", size = 25) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  #scale_y_continuous(limits = c(-4, 22), breaks = seq(-4, 22, 5))+
  #scale_x_discrete(labels = c("Females","Males"))+
  xlab("")+ ylab("Proportion of variance explained")

ch

## Adolescence
df_ado <- df_bp[df_bp$dev == "Adolescence",]

ado <- ggplot(df_ado, aes(x = bp,
                        y = est, fill=comp))+
  coord_flip()+
  ggtitle("Adolescence")+
  geom_bar(stat = "identity", color = "black", position = "stack", width = .9, size = .5)+
  facet_wrap(df_ado$rater)+
  #facet_grid(df$dev, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15,colour="black", family = "sans"),
        
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = Sig), hjust = 0, vjust = 0)+
  #geom_text(aes(label = Sig), hjust = -25, vjust = .7, colour="black", size = 25) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  #scale_y_continuous(limits = c(-4, 22), breaks = seq(-4, 22, 5))+
  #scale_x_discrete(labels = c("Females","Males"))+
  xlab("")+ ylab("Proportion of variance explained")

ado

## Adulthood
df_adu <- df_bp[df_bp$dev == "Adulthood",]

adu <- ggplot(df_adu, aes(x = bp,
                          y = est, fill=comp))+
  coord_flip()+
  ggtitle("Adulthood")+
  geom_bar(stat = "identity", color = "black", position = "stack", width = .9, size = .5)+
  facet_wrap(df_adu$rater)+
  #facet_grid(df$dev, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15,colour="black", family = "sans"),
        
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = Sig), hjust = 0, vjust = 0)+
  #geom_text(aes(label = Sig), hjust = -25, vjust = .7, colour="black", size = 25) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  #scale_y_continuous(limits = c(-4, 22), breaks = seq(-4, 22, 5))+
  #scale_x_discrete(labels = c("Females","Males"))+
  xlab("")+ ylab("Proportion of variance explained")

adu

## Combine
Fig21 = ggdraw() +
  draw_plot(pr, x = 0, y = .75, height = .25, width = 1) +
  draw_plot(ch, x = 0, y = .5, height = .25, width = 1) +
  draw_plot(ado, x = 0, y = .25, height = .25, width = 1) +
  draw_plot(adu, x = 0, y = 0, height = .25, width = 1)
#draw_plot_label(c("Whole sample", "Males vs females"), c(0.3, 0.2), c(1, 1), size = 20)

ggsave(Fig21, filename = "./Figure21.tiff", height = 10, width = 10)

##### polyE #####
## Factors
df_polyE$polyE <- factor(df_polyE$polyE, levels = c("Peer problems", "Emotional problems", "Conduct problems",
                                        "Hyperactivity"))

df_polyE$dev <- factor(df_polyE$dev, levels = c("Preschool", "Childhood", "Adolescence"))

df_polyE$comp <- factor(df_polyE$comp, levels = c("E", "C", "A"),
                        labels = c("NSE influences", "Shared environmental influences", "Genetic influences"))

## Preschool
df_pr <- df_polyE[df_polyE$dev == "Preschool",]

pr <- ggplot(df_pr, aes(x = polyE,
                        y = est, fill=comp))+
  coord_flip()+
  ggtitle("Preschool")+
  geom_bar(stat = "identity", color = "black", position = "stack", width = .9, size = .5)+
  #facet_wrap(df_pr$rater)+
  #facet_grid(df_pr$rater, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15,colour="black", family = "sans"),
        
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = Sig), hjust = 0, vjust = 0)+
  #geom_text(aes(label = Sig), hjust = -25, vjust = .7, colour="black", size = 25) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  #scale_y_continuous(limits = c(-4, 22), breaks = seq(-4, 22, 5))+
  #scale_x_discrete(labels = c("Females","Males"))+
  xlab("")+ ylab("Proportion of variance explained")

pr

## Childhood
df_ch <- df_polyE[df_polyE$dev == "Childhood",]

ch <- ggplot(df_ch, aes(x = polyE,
                        y = est, fill=comp))+
  coord_flip()+
  ggtitle("Childhood")+
  geom_bar(stat = "identity", color = "black", position = "stack", width = .9, size = .5)+
  #facet_wrap(df_ch$rater)+
  #facet_grid(df$dev, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15,colour="black", family = "sans"),
        
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = Sig), hjust = 0, vjust = 0)+
  #geom_text(aes(label = Sig), hjust = -25, vjust = .7, colour="black", size = 25) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  #scale_y_continuous(limits = c(-4, 22), breaks = seq(-4, 22, 5))+
  #scale_x_discrete(labels = c("Females","Males"))+
  xlab("")+ ylab("Proportion of variance explained")

ch

## Adolescence
df_ado <- df_polyE[df_polyE$dev == "Adolescence",]

ado <- ggplot(df_ado, aes(x = polyE,
                          y = est, fill=comp))+
  coord_flip()+
  ggtitle("Adolescence")+
  geom_bar(stat = "identity", color = "black", position = "stack", width = .9, size = .5)+
  #facet_wrap(df_ado$rater)+
  #facet_grid(df$dev, space = "free", scales = "free")+
  theme(legend.position='none',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15,colour="black", family = "sans"),
        
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = Sig), hjust = 0, vjust = 0)+
  #geom_text(aes(label = Sig), hjust = -25, vjust = .7, colour="black", size = 25) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  #scale_y_continuous(limits = c(-4, 22), breaks = seq(-4, 22, 5))+
  #scale_x_discrete(labels = c("Females","Males"))+
  xlab("")+ ylab("Proportion of variance explained")

ado

## Combine
Fig22 = ggdraw() +
  draw_plot(pr, x = 0, y = .66, height = .33, width = 1) +
  draw_plot(ch, x = 0, y = .33, height = .33, width = 1) +
  draw_plot(ado, x = 0, y = 0, height = .33, width = 1)
#draw_plot_label(c("Whole sample", "Males vs females"), c(0.3, 0.2), c(1, 1), size = 20)

ggsave(Fig22, filename = "./Figure22.tiff", height = 10, width = 10)

##### Combine BP and polyE #####
Fig2 = ggdraw() +
  draw_plot(Fig21, x = 0, y = 0, height = 1, width = .6) +
  draw_plot(Fig22, x = .6, y = .4, height = .6, width = .4) 
#draw_plot_label(c("Whole sample", "Males vs females"), c(0.3, 0.2), c(1, 1), size = 20)

ggsave(Fig2, filename = "./Figure2.tiff", height = 10, width = 15)

##### Get legend #####
ado <- ggplot(df_ado, aes(x = polyE,
                          y = est, fill=comp))+
  coord_flip()+
  ggtitle("Adolescence")+
  geom_bar(stat = "identity", color = "black", position = "stack", width = .9, size = .5)+
  #facet_wrap(df_ado$rater)+
  #facet_grid(df$dev, space = "free", scales = "free")+
  theme(legend.position='right',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15,colour="black", family = "sans"),
        
        strip.background = element_rect(colour="black", fill="white"),
        panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.border = element_blank(),panel.grid=element_blank(),
        plot.background=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'white', size=0.5, linetype='solid'),
        axis.text = element_text(size = 15, colour="black", family = "sans"), 
        axis.title= element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.spacing=unit(1.5, "lines"))+
  guides(fill = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = Sig), hjust = 0, vjust = 0)+
  #geom_text(aes(label = Sig), hjust = -25, vjust = .7, colour="black", size = 25) +
  scale_fill_manual(values = c("#f2cc8f","#3d405b","#e07a5f"))+
  #scale_y_continuous(limits = c(-4, 22), breaks = seq(-4, 22, 5))+
  #scale_x_discrete(labels = c("Females","Males"))+
  xlab("")+ ylab("Proportion of variance explained")

ado

