###--------------------------------------------###
###------------Rodrigues & Daudt 2023----------###
###---Spatial modelling in SW Atlantic Ocean---###
###-----------Ecological Modelling-------------###
###----------Updated August 22 2023------------###
###--------------------------------------------###

# clean global environment
rm(list=ls())

# libraries
library(bibliometrix)
library(readxl)
library(ggplot2)
remotes::install_github("nrennie/ggflowchart")
library(ggflowchart)

# import .bib data from WoS and convert in a dataframe
WoS <- convert2df("savedrecs_09_05_23.bib", dbsource = "wos", format = "bibtex") 

# extract the main bibliometric indicators fro WoS dataframe
results_WoS <- biblioAnalysis(WoS, sep = ";")

# import .bib data from Scopus and convert in a dataframe
Scopus <- convert2df("scopus_09_05_23.bib", dbsource = "scopus", format = "bibtex") 

# extract the main bibliometric indicators fro Scopus dataframe
results_Scopus <- biblioAnalysis(Scopus, sep = ";")

# combine both data databases and remove duplicates
# please note that we have previously identified the duplicate documents 
# they were downloaded solely through the WoS platform
comb_WoS_Sco <- mergeDbSources(WoS, Scopus, remove.duplicated = T)

# extract the main bibliometric indicators fro both dataframes
results_WoS_Sco <- biblioAnalysis(comb_WoS_Sco, sep = ";")

# an overview of data
summary(results_WoS_Sco)

###--------------------------------------------###
# Yearly/Cumulative scientific production plot
# production by year
new = c(1,0,1,2,2,4,3,7,4,3,1,4,8,11,11)

#cumulative
cumulative <- cumsum(new)

# preparing lines cumulative data
line_cumulative <- data.frame(year = c(min(results_WoS_Sco$Years):max(results_WoS_Sco$Years),
                                      min(results_WoS_Sco$Years):max(results_WoS_Sco$Years)),
                              categ = c(rep('new',length(new)),rep('cumulative',length(new))),
                              freq = c(new,cumulative))

# importing data for stack barplot
stack_bar_yearly <- read_excel("data_stack.xlsx", sheet = "Plan1")
stack_bar_yearly$year <- as.numeric(stack_bar_yearly$year)

# yearly and cumulative scientific production plot by focus of research
# Figure 1
ggplot()  + 
  geom_bar(data=stack_bar_yearly, aes(x=year, y=count, fill=focus),stat="identity")+
  geom_point(data=line_cumulative[line_cumulative$categ=="cumulative",], aes(x=year, y=cumulative/4),stat="identity",color="black",size=4)+
  geom_line(data=line_cumulative[line_cumulative$categ=="cumulative",], aes(x=year, y=cumulative/4),stat="identity",color="black",size=2)+
  scale_fill_manual(values=c("#ef8a62", "#fddbc7", "#d1e5f0", "#67a9cf", "#2166ac"))+
  scale_y_continuous(sec.axis=sec_axis(~.*4,name="Cumulative (dotted line)"))+
  scale_x_continuous(breaks = seq(min(results_WoS_Sco$Years),max(results_WoS_Sco$Years),2))+
  ylab("Yearly (bars)")+
  xlab("Year")+
  theme_bw()

# Figure 2 created out of code

# Scientific production by type of biological data
# production 
bio_data <- data.frame(categ = c("half","presence-only","catch rate","presence-absence","count"),
                       freq = c(57, 41, 8, 5, 3))

# adjustments
bio_data$categ <- factor(bio_data$categ, levels = c("half","presence-only","catch rate","presence-absence","count"))
mycols <- c("black", "#ae017e", "#f768a1","#fbb4b9","#feebe2")

# Figure 3a
ggplot(bio_data, aes(x = 1.5, y = freq, fill = categ)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)+
  theme(legend.position = "none")

# Scientific production by type of environmental data
# production
env_data <- data.frame(categ = c("half","abiotic_biotic_n-i","abiotic","biotic_biotic_n-i_anthop"),
                       freq = c(57, 40, 16, 1))

# adjustments
env_data$categ <- factor(env_data$categ, levels = c("half","abiotic_biotic_n-i","abiotic","biotic_biotic_n-i_anthop"))
mycols <- c("black", "#31a354", "#addd8e", "#f7fcb9")

# Figure 3a
ggplot(env_data, aes(x = 1.5, y = freq, fill = categ)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)+
  theme(legend.position = "none")

# Scientific production by analytical methods
# production
method_data <- data.frame(method = c("MaxEnt","GLM/GAM","BRT","RF","Bioclim",
                                     "Mahalanobis Distance","SVM","ANN","SRE",
                                     "MARS","Others"),
                          count = c(35,22,6,4,3,3,3,2,2,2,9))

# adjustments
method_data$freq <- method_data$count/62
method_data$method <- factor(method_data$method, levels = c("MaxEnt","GLM/GAM","BRT","RF","Bioclim",
                                                            "Mahalanobis Distance","SVM","ANN","SRE",
                                                            "MARS","Others"))
# Figure 3b
ggplot(method_data, aes(x=freq*100, y=method)) +
  geom_bar(stat="identity", size=2, position=position_dodge(), fill="#ef8a62")+theme_minimal()+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 0, vjust = 0.3, hjust = 0.5), 
        axis.text.y = element_text(colour = "black", face = "bold", size = 11))+ 
  scale_y_discrete(limits = rev)+
  ylab("")+
  xlab("Frequency (%)") +
  xlim(0,60)+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.x = element_text(size = 14))

# Collaborative network between countries
# extracting information from .bib data
M <- metaTagExtraction(comb_WoS_Sco, Field = "AU_CO", sep = ";")

# creating network matrix between countries
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries",short = T, sep = ";")

# Figure 4a created out of code

# Figure 4b
net=networkPlot(NetMatrix, n = nrow(NetMatrix), halo=F, cluster = "optimal", normalize = T,
                labelsize = 1,label = T, size.cex = T, curved = 0.1,
                Title = "Country Collaboration", type = "mds", 
                size=TRUE,remove.multiple=F, alpha = .8, edgesize = 10, label.n = nrow(NetMatrix),
                community.repulsion	= 10,  
                noloops = T, remove.isolates = T, verbose = T, edges.min = 1)


# creating network matrix between authors
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors",short = T, sep = ";")

# Figure 4c
net=networkPlot(NetMatrix, n = 30, halo=F, cluster = "optimal", normalize = T,
                labelsize = .8,label = T, size.cex = T, curved = 0.1,
                Title = "Author Collaboration", type = "mds", 
                size=TRUE,remove.multiple=F, alpha = .8, edgesize = 10, label.n = nrow(NetMatrix),
                community.repulsion	= 10,  
                noloops = T, remove.isolates = T, verbose = T, edges.min = 1)

# Flowchart of systematic review based on PRISMA elements
# creating flowchart
# code adapted from Nicola Rennie
# available in https://www.r-bloggers.com/2022/06/creating-flowcharts-with-ggplot2/
# libraries
library(tidyverse)
library(igraph)
library(showtext)
library(rcartocolor)
library(dplyr)

# identifying nodes
lv1a <- "107 identified from WoS"
lv1b <- "97 identified from Scopus"
lv2a <- "84 duplicates in both databases"
lv2b <- "120 records screened"
lv2c <- "6 excluded by inaccessability"
lv3a <- "114 assessed for eligibility"
lv3b <- "17 excluded by 3rd criterion" # criteria 1: carried out outside SWAO boundaries
lv3c <- "6 excluded by 2nd criterion" # criteria 2: did not include marine species modelling
lv3d <- "29 excluded by 1st criterion" # criteria 3: were not in the scope of SDM
lv4 <- "62 included"

# creating data
data <- tibble::tibble(from = c(lv1a, lv1b, lv2a, lv2b, lv2b, lv3a, lv3a, lv3a, lv3a),
                         to = c(lv2a, lv2a, lv2b, lv2c, lv3a, lv3b, lv3c, lv3d, lv4))
# data transformation
g = graph_from_data_frame(data, directed = TRUE)
coords = layout_as_tree(g)
colnames(coords) = c("x", "y")
output_df = as_tibble(coords) %>%
  mutate(step = vertex_attr(g, "name"),
         label = gsub("\\d+$", "", step),
         x = x*-1,
         type = factor(c("Identification", "Identification", "Screening", "Screening", "Eligibility", "Eligibility", "Eligibility","Eligibility","Eligibility", "Included")))
output_df

# data for nodes - change the sizes of boxes
plot_nodes = output_df %>%
  mutate(xmin = x - 0.4,
         xmax = x + 0.4,
         ymin = y - 0.25,
         ymax = y + 0.25)
plot_nodes

# data for arrows
plot_edges = data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c("from", "to"),
               names_to = "s_e",
               values_to = "step") %>%
  left_join(plot_nodes, by = "step") %>%
  select(-c(label, type, y, xmin, xmax)) %>%
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  select(-c(ymin, ymax))

# adjustment
plot_nodes$type <- factor(plot_nodes$type, levels = c("Identification", "Screening", "Eligibility", "Included"))

# figure S1
ggplot() +
  geom_rect(data = plot_nodes,
            mapping = aes(xmin = xmin, ymin = ymin, 
                          xmax = xmax, ymax = ymax, 
                          fill = type, colour = type),
            alpha = 0.5) +
  geom_text(data = plot_nodes,
            mapping = aes(x = x, y = y, label = label),
            family = "Times", size = 2,
            color = "#585c45") +
  geom_path(data = plot_edges,
            mapping = aes(x = x, y = y, group = id),
            colour = "#585c45",
            arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "left") +
  guides(colour=guide_legend(title="Step"), fill = guide_legend(title="Step"))

#ggsave("image.jpeg", width = 180, height = 120, units = c("mm"), dpi = 600)

# figure 5
# libraries
library(readxl)
library(car)
library(ggplot2)
library(emmeans)
library(dplyr)
library(lme4)

# insert data
data <- read_excel("RodriguesDaudt_etal_SDM_SWAO_BGLM_data.xlsx", sheet = "Plan1")

# frequentist Binomial model
mod <- glm(cbind(excellent, total-excellent) ~ class, data = data, family = binomial(link="logit"))
Anova(mod)

# generating data from model
yhat.df <- emmeans(mod, ~ class, at=list(class=seq(min(data$class),max(data$class),by=ifelse(max(data$class)>1000,1,.1))), type='response') %>%
  as.data.frame()

# choose the cutoff! ex. .5, .9, etc
prob_excel1 <- .5
prob_excel2 <- .9
# class at excellency
cut1 <- filter(yhat.df, prob >= prob_excel1)
cut2 <- filter(yhat.df, prob >= prob_excel2)

classexcel1 <- round(min(cut1$class),3)
classexcel1
classexcel2 <- round(min(cut2$class),3)
classexcel2
# plot
ggplot() +
  geom_line(data = yhat.df, aes(y = asymp.LCL, x = class), col="grey", lty=1, lwd=.8) +
  geom_line(data = yhat.df, aes(y = asymp.UCL, x = class), col="grey", lty=1, lwd=.8) +
  geom_line(data = yhat.df, aes(y = prob, x = class), col="black", lty=1, lwd=1) +
  geom_segment(aes(x = -15, xend = classexcel1, y = prob_excel1, yend = prob_excel1), color ="#ef8a62", size =.8, linetype = "longdash")+
  geom_segment(aes(x = classexcel1, xend = classexcel1, y = -.5, yend = prob_excel1), color ="#ef8a62", size =.8, linetype = "longdash")+
  geom_segment(aes(x = -15, xend = classexcel2, y = prob_excel2, yend = prob_excel2), color ="#2166ac", size =.8, linetype = "longdash")+
  geom_segment(aes(x = classexcel2, xend = classexcel2, y = -.5, yend = prob_excel2), color ="#2166ac", size =.8, linetype = "longdash")+
  geom_point(data = data[-1,], aes(x = class, y = excellent/total, size = total), colour = "black", shape = 16)+
  scale_size(name = "No of outputs", range = c(4,10), breaks = c(7, 17, 65)) +
  annotate(geom = "text",x = quantile(seq(min(data$class),max(data$class)),.9), y = .9, label = bquote(Layer[50] %~~% "5-8"), color = "#ef8a62") +
  annotate(geom = "text",x = quantile(seq(min(data$class),max(data$class)),.9), y = .85, label = bquote(Layer[90] %~~% "13-16"), color = "#2166ac") +
  geom_point(aes(x = 0, y = 0), colour = "red3", shape = 4, stroke=2) +
  ylab("Probability of excellent performance (AUC \u2265 0.9)") +
  xlab("No of layers") +
  scale_x_continuous(breaks = as.numeric(data$class), labels=c("0", "1-4", "5-8", "9-12",
                                                               "13-16", "17-20", "21-24",
                                                               "32", "34")) +
  coord_cartesian(xlim = c(min(data$class),max(data$class)), ylim = c(0,1))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.1, linetype = 2), legend.position = c(0.85,0.25),
        legend.background = element_blank())

# saving plot
#ggsave("image.jpeg", width = 120, height = 100, units = c("mm"), dpi = 600)

###-----------End of Analysis-------------###