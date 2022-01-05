
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Accompanying code for the data paper:
#
# "Historical records of plant-insect interactions in subarctic Finland "
#
# authored by L. Zoller and T. M. Knight
# 
#
# Script authored by L. Zoller
# Please refer to the paper for detailed methodological explanations and relevant citations.
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Attach packages and data: ----

library(xlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(rJava)
library(tidyverse)
library(car)
library(scico) 
library(cowplot)

# read in data
dat <- read.xlsx("data/211119_data_Silen_original.xlsx", 2)
setDT(dat)

#convert character strings to factors:
dat <- strings2factors(dat) 
nrow(dat)                                            # 654 rows in dataset

# The dataset contains 654 rows. Each row represents a unique 
# plant-insect interaction at a specific site and date (hereafter called "record")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Taxonomic coverage of the dataset ----

# verbatim taxonomy (as originally stated in the historic dataset):
# Count the number of unique verbatim plant and insect taxa represented in the dataset
length(levels(dat$plantVerbatimIdentification))      #  86 verbatim plant species
length(levels(dat$animalVerbatimIdentification))     # 187 verbatim insect taxa

# create a new variable "verbatimInteraction" reflecting the interaction between verbatim plant  
# and verbatim insect taxa and count the number of unique verbatim interactions:
dat <- dat %>%
       unite("verbatimInteraction", 
             plantVerbatimIdentification, animalVerbatimIdentification,
             sep = " : ", remove =F)
length(levels(as.factor(dat$verbatimInteraction)))   # 503 original unique plant-insect interactions


# updated taxonomy:

# create a new variable "Interaction" reflecting the interaction between validated plant  
# and insect taxa and count the number of unique interactions:
dat <- dat %>%
  unite("Interaction", 
        plantScientificName, animalScientificName,
        sep = " : ", remove =F)

# count the number of unique observations using the updated taxonomy
length(levels(dat$plantScientificName))              #  86 plant species
length(levels(dat$animalScientificName))             # 174 insect taxa
length(levels(as.factor(dat$Interaction)))           # 498 original unique plant-insect interactions

# count the number and proportion of records involving insect taxa resolved to species, genus, family and order level
dat_species <- dat %>% 
   filter(animalTaxonRank ==  ("species"))
length(unique(dat_species$animalScientificName))     # 153 taxa resolved to species level
length(dat_species$animalScientificName)             # 617 records contain insects resolved to species level
length(dat_species$animalScientificName)/nrow(dat)   # 94.34% of records contain insects resolved to species level

dat_genus <- dat %>% 
  filter(animalTaxonRank ==  ("genus"))
length(unique(dat_genus$animalScientificName))       # 13 taxa resolved to species level
length(dat_genus$animalScientificName)               # 17 records contain insects resolved to genus level
length(dat_genus$animalScientificName)/nrow(dat)     # 2.60% of records contain insects resolved to genus level

dat_family <- dat %>% 
  filter(animalTaxonRank ==  ("family"))
length(unique(dat_family$animalScientificName))      #  7 taxa resolved to species level
length(dat_family$animalScientificName)              # 14 records contain insects resolved to family level
length(dat_family$animalScientificName)/nrow(dat)    # 2.14% of records contain insects resolved to family level

dat_order <- dat %>% 
  filter(animalTaxonRank ==  ("order"))
length(unique(dat_order$animalScientificName))       # 1 taxon resolved to species level
length(dat_order$animalScientificName)               # 6 records contain insects resolved to family level
length(dat_order$animalScientificName)/nrow(dat)     # 0.92% of records contain insects resolved to family level


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# * Visualize the most commonly recorded plant and insect families and interaction combinations (Fig. 2) ----

#remove entries with families as NA:
dat_noNA <- dat %>% drop_na(animalFamily)

# create separate data tables summing up the number of records per insect and plant families
no_insect_families <- dat_noNA %>%
  group_by(animalFamily) %>%
  summarise(observations = n())
setDT(no_insect_families)

no_plant_families <- dat_noNA %>%
  group_by(plantFamily) %>%
  summarise(observations = n())
setDT(no_plant_families)

# create an interaction matrix of interactions between insect families and plant species
interaction_numbers <- tapply(dat_noNA$animalScientificName, list(dat_noNA$animalFamily, 
                                                                  dat_noNA$plantFamily), length)
interaction_numbers[is.na(interaction_numbers)] <- 0

# melt interaction matrix into a data frame:
int_num <- reshape2::melt(interaction_numbers)
colnames(int_num) <- c("insect_family", "plant_family", "No_interactions")

# plot a heatmap of the number of interactions
assoc_heatmap <- ggplot(int_num, aes(insect_family, plant_family, fill = (No_interactions))) + 
  geom_tile() +
  scale_fill_scico(name="No. interactions", palette = 'bilbao', begin = 0.05) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7), 
        axis.text.y = element_text(size = 7), 
        legend.direction = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 7)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

# get the legend separately
tmp <- ggplot_gtable(ggplot_build(assoc_heatmap))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]

# remove the legend from the heatmap
assoc_heatmap_clean <- assoc_heatmap +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position ="none")

# create bar graphs counting the number of records of insect and plant families
barcols <- scico(33, palette = 'bilbao')[28]  # chose color
insect_barplot <- ggplot(no_insect_families, aes(x = animalFamily, y = observations)) +
  geom_bar(stat = "identity", fill = barcols) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(size = 20, margin = margin(10,0,0,0)),
        axis.ticks.y = element_blank(),
        legend.position = "none") + 
  labs(x = "Insect family", y = "No. records")

plant_barplot <- ggplot(no_plant_families, aes(x = plantFamily, y = observations)) +
  geom_bar(stat = "identity", fill = barcols) +
  theme_minimal() +
  scale_y_continuous() +
  theme(axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(0,10,0,0), angle = -90),
        legend.position= "none") +
  coord_flip() + 
  labs(x = "Plant family", y = "No. records")

#arrange separate plots into one
interactionmap_all <- plot_grid(insect_barplot, legend,assoc_heatmap_clean, plant_barplot,
          ncol = 2,
          align = "vh", axis = "tbl", rel_widths = c(1, 0.5), rel_heights = c(0.7, 1))

interactionmap_all

# ggsave(path = "output/",
#       filename = paste("Fig. 2", ".png"),
#       width =  17, height = 17, units ="cm", dpi = 300,
#       device = "png") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# * Visualize the number of records in the data set by insect genera and plant species (Fig. 3) ----

# insect genera: 
#remove entries with families as NA:
dat_noNA <- dat %>% 
  drop_na(animalGenericName) %>% 
  drop_na(plantScientificName)

# create data tables with counts of  number of records per insect genus and plant species
insect_gen <- dat_noNA %>%
  group_by(animalGenericName) %>%
  summarise(observations = n())
setDT(insect_gen)

barcols <- scico(33, palette = 'bilbao')[28]  # chose color

insect_genus_barplot <- ggplot(insect_gen, aes(x= reorder(animalGenericName, -observations), observations)) +
  geom_bar(stat = "identity", fill = barcols) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7.3, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin = margin(10,0,0,0)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 7.3),
        legend.position = "none") + 
  coord_flip() + 
  labs(x = "Insect genera", y = "No. records")

# plant species: 
# create data tables with counts of  number of records per insect genus and plant species

# create a new variable "plantBinomen" combining the generic name and specific Epithet of the plant
dat <- dat %>%
  unite("plantBinomen", 
        plantGenericName, plantSpecificEpithet,
        sep = " ", remove =F)

plant_spec <- dat %>%
  group_by(plantBinomen) %>%
  summarise(observations = n())
setDT(plant_spec)

plant_spec_barplot <- ggplot(plant_spec, aes(x = reorder(plantBinomen, -observations), observations)) +
  geom_bar(stat = "identity", fill = barcols) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6.7, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(10,0,0,0)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 6.7),
        legend.position = "none") + 
  coord_flip() + 
  labs(x = "Plant species", y = "No. records")

NrRecords_all <- plot_grid(insect_genus_barplot, plant_spec_barplot,
                                align = "vh", axis = "tbl", rel_widths = c(1, 1))
NrRecords_all

# ggsave(path = "output/",
#       filename = paste("Fig. 3", ".png"),
#       width =  17, height = 17, units ="cm", dpi = 300,
#       device = "png") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Temporal coverage of the dataset ----
# * Visualize the number of records in the dataset by year and month (Fig. 4) ----

#remove entries with years or months as NA:
dat_noNA <- dat %>% 
  drop_na(year) %>% 
  drop_na(month)

# create data tables with counts of number of records per year and month
# year:
counts_year <- dat_noNA %>%
  group_by(year) %>%
  summarise(observations = n())
counts_year$year <- as.factor(counts_year$year)
setDT(counts_year)

barcols <- scico(33, palette = 'bilbao')[28]  # chose color

year_barplot <- ggplot(counts_year, aes(x = year, y = observations)) +
  geom_bar(stat = "identity", fill = barcols) +
  theme_minimal() +
  theme(axis.text.x = element_text( margin = margin(-10, 0, 0, 0)),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") + 
  labs(y = "No. records")


# months 
counts_month <- dat_noNA %>%
  group_by(month) %>%
  summarise(observations = n())
counts_month$month <- as.factor(counts_month$month)
setDT(counts_month)

month_barplot <- ggplot(counts_month, aes(x = month, y = observations)) +
  geom_bar(stat ="identity", fill = barcols)+
  theme_minimal() +
  theme(axis.text.x = element_text(margin = margin(-10, 0, 0, 0)),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") + 
  scale_x_discrete(labels=c("May", "June", "July", "August")) +
  labs(y = "No. records")


Temp_coverage_all <- plot_grid(year_barplot, month_barplot,
                               align = "vh", axis = "tbl", rel_widths = c(1, 1))
Temp_coverage_all

 # ggsave(path = "output/",
 #     filename = paste("Fig. 4", ".png"),
 #     width =  14, height = 10, units = "cm", dpi = 300,
 #     device = "png")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

