# ------------------------------------------------------------------------
# Data wrangling with the Tidyverse

# Code used to illustrate key functions and operations from the 
# core Tidyverse packages. 

# Data are based on waterbird counts conducted in KwaZulu-Natal at 
# 60 sites over several successive months (missions). Each site falls 
# within a wetland cluster and has a protection status (full, partial, 
# non-protected). Habitat and weather measurements were taken after 
# each point count. 
# ------------------------------------------------------------------------

install.packages("tidyverse")
#setwd("C:/Users/01466539/Desktop/R Learning/R immediate workshop/Tidyverse")

# Install and load libraries ----------------------------------------------
# install.packages("tidyverse")
#install.packages("here", dependencies = TRUE)

install.packages("tidyverse")

library(tidyverse)

setwd("C:/Users/01466539/Desktop/R Learning/R immediate workshop/Tidyverse")

# Import data by setting your own working directory -----------------------
setwd("C:/Users/01466539/Desktop/R Learning/R immediate workshop/Tidyverse")

counts1 = read.csv

counts1 <- read.csv("bird_counts.csv")
str(counts1)
head(counts1)


counts <- readr::read_csv("bird_counts.csv")

counts <- read_csv("bird_counts.csv")

# Tibble ------------------------------------------------------------------
counts  # Say goodbye to head()!


print(counts, n = 20)
print(counts, width = Inf)

# Tidy column names -------------------------------------------------------
counts <- dplyr::rename_all(counts, tolower)
counts

###select columns

# Select & reorder --------------------------------------------------------
counts <- dplyr::select(counts, site,protection,year,month,abundance,richness,
                        everything())

names(counts)

olina1 <- dplyr::select(counts, -matches("sl_|aq_"),-humidity)
names(olina1)

# Split site variable -----------------------------------------------------
counts <- tidyr::separate(counts, site, c("mission","cluster","site"), sep = "_")
counts

# Arrange(Putting them in order) -----------------------------------------------------------------
counts <- dplyr::arrange(counts, mission, cluster,site)
counts

# Mutate ------------------------------------------------------------------
counts <- dplyr::mutate(counts, year = year + 10)
counts

counts <- dplyr::mutate(counts, air_temp = (air_temp - 32)/1.8)
counts

# What about the "$"?

# Filter ------------------------------------------------------------------
counts[["ph"]]
counts <-  dplyr::filter(counts,!is.na(ph))
counts

# All together ------------------------------------------------------------
counts <- read_csv("bird_counts.csv") %>% 
  rename_all(tolower) %>% 
  select(site,protection,year,month,abundance,richness,everything()) %>% 
  select(-matches("sl_|aq_"), -humidity) %>% 
  separate(site, c("mission","cluster","site"), sep = "_") %>% 
  arrange(mission, cluster, site) %>% 
  mutate(year = year + 10,air_temp = (air_temp - 32)/1.8) %>% 
  filter(!is.na(ph))

counts # Success!


# Mutate multiple columns -------------------------------------------------
counts %>% 
  select(ph:sal)

counts %>% 
  mutate_at(vars(ph:sal),scale) %>% 
  select(ph:sal)

## Base R ##
as.data.frame(apply(counts[,16:20], 2, function(x) scale (x)))


# Group and summarise -----------------------------------------------------

counts %>% 
  group_by(cluster) %>% 
  summarise(mean(abundance),sd(abundance))

# Group, summarise and plot -----------------------------------------------
counts %>% 
    mutate(year = as.factor(year)) %>% 
    group_by(cluster,year) %>% 
    summarise(mean_rich = mean(richness),sd_rich = sd(richness)) %>% 
    ggplot(aes(x = cluster, y = mean_rich, fill = year))+
    geom_bar(stat = "identity",position = position_dodge())+
    geom_errorbar(aes(ymin=mean_rich, ymax=mean_rich + sd_rich),
                                    width=.2,position=position_dodge(.9))
# Reorder factor levels ---------------------------------------------------
counts %>% 
  group_by(cluster) %>% 
  summarise(mean_abun = mean(abundance)) %>% 
  ggplot(aes(x = fct_reorder(cluster,mean_abun), y = mean_abun))+
  geom_bar(stat = "identity")

## Forcats ##
#fct_reorder(cluster,mean_abun)
## Base R ##

#counts$cluster <-factor(counts$cluster, levels = counts[order(counts$mean_abun),"cluster"])


# Filter and summarise ----------------------------------------------------
counts %>% 
  filter(protection == "FP" & air_temp < 30) %>% 
  group_by(cluster, year) %>% 
  summarise(total = sum(abundance), max = max(abundance),n_counts = n()) %>% 
  filter(total > 500)


