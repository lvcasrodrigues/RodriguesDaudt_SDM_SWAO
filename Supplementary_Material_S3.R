###--------------------------------------------###
###------------Rodrigues & Daudt 202x----------###
###---Spatial modelling in SW Atlantic Ocean---###
###-----------Ecological Modelling-------------###
###-----------Updated May 23 2023--------------###
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
# Figure 1a
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

# Figure 1b created out of code

# Scientific production by type of biological data
# production 
bio_data <- data.frame(categ = c("half","presence-only","catch rate","presence-absence","count"),
                       freq = c(57, 41, 8, 5, 3))

# adjustments
bio_data$categ <- factor(bio_data$categ, levels = c("half","presence-only","catch rate","presence-absence","count"))
mycols <- c("black", "#ae017e", "#f768a1","#fbb4b9","#feebe2")

# Figure 1c
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

# Figure 1c
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
# Figure 1d
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

# Figure 1e created out of code

# Figure 1f
net=networkPlot(NetMatrix, n = nrow(NetMatrix), halo=F, cluster = "optimal", normalize = T,
                labelsize = 1,label = T, size.cex = T, curved = 0.1,
                Title = "Country Collaboration", type = "mds", 
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
lv2c <- "2 excluded by inaccessability"
lv3a <- "118 assessed for eligibility"
lv3b <- "56 excluded (out of scopus)"
lv4 <- "62 included"

# creating data
data <- tibble::tibble(from = c(lv1a, lv1b, lv2a, lv2b, lv2b, lv3a, lv3a),
                         to = c(lv2a, lv2a, lv2b, lv2c, lv3a, lv3b, lv4))
# data transformation
g = graph_from_data_frame(data, directed = TRUE)
coords = layout_as_tree(g)
colnames(coords) = c("x", "y")
output_df = as_tibble(coords) %>%
  mutate(step = vertex_attr(g, "name"),
         label = gsub("\\d+$", "", step),
         x = x*-1,
         type = factor(c("Identification", "Identification", "Screening", "Screening", "Eligibility", "Eligibility", "Eligibility", "Included")))
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
            family = "henny", size = 2.5,
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
###-----------End of Analysis-------------###