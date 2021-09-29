#### Term Paper QTA
## Summer Semester 2020
# Descriptives

# Clean environment
rm(list = ls())
Sys.setenv(LANG = "en", LC_CTYPE = "en_US.UTF-8")
options(stringsAsFactors = FALSE, na.rm = T)

# Author
(author <- data.frame(name = "Cornelius Erfort", email = "erfortcx@hu-berlin.de", stringsAsFactors = F))

# HTTP header
(header <- c(From = author$email,`User-Agent` = R.Version()$version.string))

# Working directory
setwd("/Volumes/Macintosh HD/Users/cornelius/Desktop/HU SOWI/MA/QTA/Term Paper")

# Load packages
source("packages.R")


# Functions
make.folder <- function(folder) if(!dir.exists(folder)) dir.create(folder, recursive = T)

make.folder("Latex/figure")
make.folder("Latex/table")


#########################################################################################

###########
# Loading the data
###########


# Load the events data
load("data.nosync/Anfragen/events_final.RData")
events <- select(events, -c(index, Land, Ort, file, table_id, Anmelder, year, month, day))
names(events) <- str_c(names(events), ".gvt")

# Load the poldem6 data
load("data.nosync/POLDEM/poldem6.RData")
poldem6 <- select(poldem6, c(title, date, poldem_part, name, geocode, latitude, longitude, match, govt_id, govt_Ort, poldem_saturation, west, weekend))
names(poldem6) <- str_c(names(poldem6), ".6")

# Load the poldem30 data
load("data.nosync/POLDEM/poldem30.RData")
poldem30 <- select(poldem30, c(community, id_event, doc_source, event_n, date, poldem_part, name, geocode, latitude, longitude, match, govt_id, govt_Ort, poldem_saturation, state, west, weekend))
names(poldem30) <- str_c(names(poldem30), ".30")

# Load the combined data
load("data.nosync/all_data.RData")



###########
# Summary statistics
###########

stargazer(all_data, 
          title = "Summary statistics for the dataset",
          omit = c("year", "month", "day", "latitude", "longitude", "spontan", "is_duplicate", "id_event.30", "event_n.30", "state.30"),
          covariate.labels = c("Participants (govt.)", "West Germany (govt.)", "Saturation (govt., -150d, r<40km)", "Weekend (govt.)",
                               "Participants (handcoding)", "Saturation (handcoding, $\\pm$50d, r<40km)", "West Germany (handcoding)",
                               "Participants (semi-supervised)", "Saturation (semi-supervised, $\\pm$50d, r<40km)", "Document probability (semi-supervised)", "West Germany (semi-supervised)", "Weekend (semi-supervised)", 
                               "Covered by government data", "Covered by handcoding",  "Covered by semi-supervised"),
          omit.summary.stat = c("p25", "p75"),
          digits = 2, 
          label = "summary_stat",
          out = "Latex/table/summary_statistics.tex")


min(all_data$Datum.gvt, na.rm = T)
max(all_data$Datum.gvt, na.rm = T)
difftime(max(all_data$Datum.gvt, na.rm = T), min(all_data$Datum.gvt, na.rm = T), units = "weeks")
nrow(all_data[all_data$gvt, ]) 
nrow(all_data[all_data$gvt, ]) / as.numeric(difftime(max(all_data$Datum.gvt, na.rm = T), min(all_data$Datum.gvt, na.rm = T), units = "weeks"))

min(all_data$date.6, na.rm = T)
max(all_data$date.6, na.rm = T)
difftime(max(all_data$date.6, na.rm = T), min(all_data$date.6, na.rm = T), units = "weeks")
nrow(all_data[all_data$poldem6, ])
nrow(all_data[all_data$poldem6, ]) / as.numeric(difftime(max(all_data$date.6, na.rm = T), min(all_data$date.6, na.rm = T), units = "weeks"))


min(all_data$date.30, na.rm = T)
max(all_data$date.30, na.rm = T)
difftime(max(all_data$date.30, na.rm = T), min(all_data$date.30, na.rm = T), units = "weeks")
nrow(all_data[all_data$poldem30, ]) 
nrow(all_data[all_data$poldem30, ]) / as.numeric(difftime(max(all_data$date.30, na.rm = T), min(all_data$date.30, na.rm = T), units = "weeks"))

summary(lm(participants.gvt ~ poldem_part.6, all_data))
summary(lm(participants.gvt ~ poldem_part.30, all_data))

corrgram::corrgram(select(all_data, c("participants.gvt", "poldem_part.6", "poldem_part.30")), upper.panel = corrgram::panel.cor, lower.panel = corrgram::panel.ellipse)

###########
# Map plots
###########

# Load Germany Map
germany <- readOGR("data.nosync/map/ref-nuts-2016-03m.shp/NUTS_RG_03M_2016_4326_LEVL_1.shp")
germany <- germany[germany$CNTR_CODE == "DE", ]
germany@data$id <- 1:nrow(germany@data)
germ.fort <- fortify(germany, region='NUTS_ID')

# Prepare plot theme: no axes and blank
GermanyMap <- ggplot() + 
  geom_map(data = germ.fort, map = germ.fort, aes(x = long, y = lat, group = group, map_id = id), fill = "white", colour = "black", size = 0.2) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), plot.background = element_rect(fill = "transparent", color = "NA"))


#################
# Govt Map
#################

# DensityPointsGovt
(DensityPointsGovt <- GermanyMap +
    geom_jitter(data = all_data %>% filter(participants.gvt <= 500), aes(x = longitude.gvt, y =  latitude.gvt, size = participants.gvt), alpha = .1, width = .19, height = .15) +
    guides(color = guide_legend(title = "Group"), size = guide_legend(title = "Participants")) +
   theme(text = element_text(family="LM Roman 10")))

# Save map as pdf
width <- 8
pdf("Latex/figure/government_events.pdf", bg = "white", width = width, height = width * .8, onefile = F)
reposition_legend(DensityPointsGovt + theme(text = element_text(family="LM Roman 10")) + facet_wrap(~ group.gvt, ncol = 3), panel = "panel-3-2", 'center')
dev.off()



######################
# Cross-coverage maps
######################

## Poldem6Govt
# DensityPointsPoldem6Govt
mapsample1 <- all_data[all_data$Datum.gvt >= (min(poldem6$date.6) - 3) & all_data$Datum.gvt <= (max(poldem6$date.6)), ] %>% filter(participants.gvt <= 500)
(DensityPointsPoldem6Govt <- GermanyMap +
    stat_density2d(data = mapsample1, aes(x = longitude.gvt %>% as.numeric, y =  latitude.gvt %>% as.numeric, fill = ..level..), fill = "dark grey", h = 1.5, n = 600, geom = "polygon", alpha = .3, show.legend = F) +
    geom_jitter(data = mapsample1, aes(x = longitude.gvt %>% as.numeric, y =  latitude.gvt %>% as.numeric, shape = ifelse(poldem6, "yes", "no"), color = ifelse(poldem6, "yes", "no")), alpha = .8, width = .19, height = .15) + 
    ggtitle("Government Data (2005-  2011)") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    labs(colour = "Covered by other dataset") +
    theme(text = element_text(family="LM Roman 10"))) 

# DensityPointsGovtPoldem6
mapsample2 <- all_data[all_data$date.6 >= (min(poldem6$date.6)) & all_data$date.6 <= (max(poldem6$date.6)), ] %>% filter(poldem_part.6 <= 500)
(DensityPointsGovtPoldem6 <- GermanyMap +
    stat_density2d(data = mapsample2, aes(x = longitude.6 %>% as.numeric, y =  latitude.6 %>% as.numeric, fill = ..level..), fill = "dark grey", h = 1.5, n = 600, geom = "polygon", alpha = .3, show.legend = F) +
    geom_jitter(data = mapsample2, aes(x = longitude.6 %>% as.numeric, y = latitude.6 %>% as.numeric, shape = ifelse(gvt, "yes", "no"), color = ifelse(gvt, "yes", "no")), alpha = .8, width = .19, height = .15) + 
    ggtitle("Handcoded News Articles (2005-  2011)") + theme(legend.position = c(.71, 0.03), plot.title = element_text(hjust = 0.5)) +
    labs(color = "Covered by other dataset", shape = "Covered by other dataset") + guides(colour = guide_legend(nrow = 1, title.position =  "left", size = 5), shape = guide_legend(nrow = 1, title.position =  "left", override.aes = list(size = 3))) +
    theme(text = element_text(family="LM Roman 10")))

# DensityPointsPoldem30Govt
mapsample3 <- all_data[all_data$Datum.gvt >= (min(poldem30$date.30) - 3) & all_data$Datum.gvt <= (max(poldem30$date.30)), ] %>% filter(participants.gvt <= 500)
(DensityPointsPoldem30Govt <- GermanyMap +
    stat_density2d(data = mapsample3, aes(x = longitude.gvt %>% as.numeric, y =  latitude.gvt %>% as.numeric, fill = ..level..), fill = "dark grey", h = 1.5, n = 600, geom = "polygon", alpha = .3, show.legend = F) +
    geom_jitter(data = mapsample3, aes(x = longitude.gvt %>% as.numeric, y =  latitude.gvt %>% as.numeric, shape = ifelse(poldem30, "yes", "no"), color = ifelse(poldem30, "yes", "no")), alpha = .8, width = .19, height = .15) + 
    ggtitle("Government Data (2005-  2015)") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    labs(color = "Covered by other dataset", shape = "Covered by other dataset") + guides(color = guide_legend(nrow = 1, title.position =  "left"), shape = guide_legend(nrow = 1, title.position =  "left")) +
    theme(text = element_text(family="LM Roman 10")))

# DensityPointsGovtPoldem30
mapsample4 <- all_data[all_data$date.30 >= (min(poldem30$date.30)) & all_data$date.30 <= (max(poldem30$date.30)), ] %>% filter(poldem_part.30 <= 500)
(DensityPointsGovtPoldem30 <- GermanyMap +
    stat_density2d(data = mapsample4, aes(x = longitude.30 %>% as.numeric, y =  latitude.30 %>% as.numeric, fill = ..level..), fill = "dark grey", h = 1.5, n = 600, geom = "polygon", alpha = .3, show.legend = F) +
    geom_jitter(data = mapsample4, aes(x = longitude.30 %>% as.numeric, y = latitude.30 %>% as.numeric, shape = ifelse(gvt, "yes", "no"), color = ifelse(gvt, "yes", "no")), alpha = .8, width = .19, height = .15) + 
    ggtitle("Semi-  supervised News Articles (2005-  2015)") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    labs(color = "Covered by other dataset", shape = "Covered by other dataset") + guides(color = guide_legend(nrow = 1, title.position =  "left"), shape = guide_legend(nrow = 1, title.position =  "left",  override.aes = list(size = 3)))  +
    theme(text = element_text(family="LM Roman 10")))

# Save all four to one pdf
height <- 14 # Change width to change size - will affect size of point!
pdf("Latex/figure/crosscoverage_map.pdf", bg = "white", height = height, width = height * 1.5 / 2)
grid.arrange(DensityPointsPoldem30Govt, DensityPointsGovtPoldem30, DensityPointsPoldem6Govt, DensityPointsGovtPoldem6, ncol = 2)
dev.off()


###########
# Density over time
###########

# Density over time
all_data$date <- all_data$Datum.gvt
all_data$date[is.na(all_data$date)] <- all_data$date.6[is.na(all_data$date)]
all_data$date[is.na(all_data$date)] <- all_data$date.30[is.na(all_data$date)]

all_data$dataset_list <- apply(all_data, MARGIN = 1, FUN = function(x) c("gvt", "6", "30")[c(x$gvt, x$poldem6, x$poldem30)])

unnest(all_data, "dataset_list")$dataset_list %>% table


width <- 8
pdf("Latex/figure/density_time.pdf", bg = "white", width = width, height = width / 16 * 9, onefile = F)
ggplot(unnest(all_data, "dataset_list")) + 
  geom_area(aes(y = ..count.., x = date, fill = dataset_list %>% factor(levels = c("gvt", "30", "6")), group = dataset_list %>% factor(levels = c("gvt", "30", "6"))), stat = "bin", binwidth = 150, color = "white") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name = "year") + 
  scale_fill_discrete(name = "Dataset", labels = c("Government", "Semi-  supervised", "Handcoding")) + 
  theme(axis.text.x = element_text(angle = 25, hjust=1)) +
  coord_cartesian(ylim=c(0, 150)) +
  geom_vline(aes(xintercept = ymd("2005-09-18")), lty = "dashed", color = "white") +
  geom_text(aes(x = ymd("2005-09-18"), y = 20), label = "Federal elections", angle = 90, color = "white", nudge_x = -75, hjust = 0) +
  geom_vline(aes(xintercept = ymd("2009-09-27")), lty = "dashed", color = "white") +
  geom_text(aes(x = ymd("2009-09-27"), y = 15), label = "Federal elections", angle = 90, color = "white", nudge_x = -75, hjust = 0) +
  geom_vline(aes(xintercept = ymd("2013-09-22")), lty = "dashed", color = "white") +
  geom_text(aes(x = ymd("2013-09-22"), y = 20), label = "Federal elections", angle = 90, color = "white", nudge_x = -75, hjust = 0) +
  geom_vline(aes(xintercept = ymd("2014-12-19")), lty = "dashed", color = "white") +
  geom_text(aes(x = ymd("2014-12-19"), y = 20), label = "PEGIDA foundation", angle = 90, color = "white", nudge_x = -75, hjust = 0) +
  geom_vline(aes(xintercept = ymd("2015-08-31")), lty = "dashed", color = "white") +
  geom_text(aes(x = ymd("2015-08-31"), y = 60), label = '"Wir schaffen das"', angle = 90, color = "white", nudge_x = -75, hjust = 0) +
  geom_vline(aes(xintercept = ymd("2017-09-24")), lty = "dashed", color = "white") +
  geom_text(aes(x = ymd("2017-09-24"), y = 5), label = "Federal elections", angle = 90, color = "white", nudge_x = -75, hjust = 0) +
  theme(text = element_text(family="LM Roman 10"))
  
dev.off()

###########
# Word cloud
###########

all_data$west <- "west"
all_data$west[!all_data$west.gvt] <- "east"
table(all_data$west)

# Word cloud by east german states
pdf("Latex/figure/wordcloud_east.pdf", bg = "white", width = 10, height = 8, onefile = F)
corpus(all_data$Motto.gvt[!all_data$west.gvt]) %>% dfm(remove = c("Spontandemonstration", stopwords("German")), tolower = F, remove_punct = T, remove_numbers = T, groups = all_data$state.gvt[!all_data$west.gvt]) %>%
  dfm_trim(min_termfreq = 3) %>% textplot_wordcloud(comparison = TRUE, max_words = 150, family = "LM Roman 10", rotation = 0)
dev.off()

pdf("Latex/figure/wordcloud_eastwest.pdf", bg = "white", width = 10, height = 8, onefile = F)
corpus(all_data$Motto.gvt) %>% dfm(remove = c("Spontandemonstration", stopwords("German")), tolower = F, remove_punct = T, remove_numbers = T, groups = all_data$west) %>%
  dfm_trim(min_termfreq = 3) %>% textplot_wordcloud(comparison = TRUE, max_words = 150, family = "LM Roman 10", rotation = 0) 
dev.off()



#############
# Citations
#############

citation() %>% toBibtex() %>% write("Latex/software.bib", append = F, sep = "\n")
RStudio.Version()$citation %>% toBibtex() %>% write("Latex/software.bib", append = T, sep = "\n")

used_pck <- c("stringr", "readxl", "dplyr", "tidyr", "plyr", "httr", "rvest", "urltools", "XML", "gnumeric", "english", "gsubfn",  "lubridate", "pdftools", "tabulizer", "ggplot2", "writexl", "ggmap", "sp", "rgdal", "stargazer", "geodist", "lemon", "gridExtra", "quanteda", "margins", "extrafont", "bibtex")

used_pck <- used_pck[used_pck != "base"]
citations <- sapply(used_pck, FUN = function (x) citation(x) %>% toBibtex)
for (i in 1:length(citations)) citations[[i]] %>% write("Latex/software.bib", append = T, sep = "\n")




########

mean(all_data$participants.gvt, na.rm = T) # 137
mean(all_data$participants.gvt[!all_data$poldem6], na.rm = T) # 134
mean(all_data$participants.gvt[all_data$poldem6], na.rm = T) # 280

mean(all_data$participants.gvt[!all_data$poldem30], na.rm = T) # 135
mean(all_data$participants.gvt[all_data$poldem30], na.rm = T) # 332

mean(all_data$participants.gvt[!all_data$poldem30], na.rm = T) # 134
mean(all_data$participants.gvt[all_data$poldem30], na.rm = T) # 332

mean(all_data$poldem_part.6, na.rm = T) # 307
mean(all_data$poldem_part.6[!all_data$gvt], na.rm = T) # 234
mean(all_data$poldem_part.6[all_data$gvt], na.rm = T) # 337

mean(all_data$poldem_part.30, na.rm = T) # 655
mean(all_data$poldem_part.30[!all_data$gvt], na.rm = T) # 684
mean(all_data$poldem_part.30[all_data$gvt], na.rm = T) # 515

sum(all_data$participants.gvt, na.rm = T) # 361,668
sum(all_data$poldem_part.6, na.rm = T) # 23,935
sum(all_data$poldem_part.30, na.rm = T) # 151,329