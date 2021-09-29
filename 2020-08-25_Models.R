#### Term Paper QTA
## Summer Semester 2020
# Models

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
poldem6 <- select(poldem6, c(title, date, poldem_part, name, geocode, latitude, longitude, match, govt_id, govt_Ort, poldem_saturation))
names(poldem6) <- str_c(names(poldem6), ".6")

# Load the poldem30 data
load("data.nosync/POLDEM/poldem30.RData")
poldem30 <- select(poldem30, c(community, id_event, doc_source, event_n, date, poldem_part, name, geocode, latitude, longitude, match, govt_id, govt_Ort, poldem_saturation, state))
names(poldem30) <- str_c(names(poldem30), ".30")

# Load the combined data
load("data.nosync/all_data.RData")


###############
# Variable list
###############

# gvt
names(all_data) %>% str_subset("\\.gvt")
# name - Full geographical location, format: "city, state, country"
# Datum - date of event
# orga - organisation (e.g. "Pro Chemnitz", "NPD-LV SN")
# Zuordnung - affiliation (e.g. "NPD/JN", "Neonazis)
# Motto - motto of protest (e.g. "„Merkel muss weg!“")
# participants - number of participants
# state - state name
# id - unique event id (some events share the same id, if they occured at several locations)
# location
# latitude (of protest)
# longitude (of protest)
# group - group var with 5 unique values ("Neonazis", "NPD", "PEGIDA & Co.", "Die RECHTE/Der 3. Weg/PRO", "other")
# geocode - all geodata from google api
# is_duplicate - the protest occured at several locations, therefore the id is a duplicate
# spontan - the protest was not registered in advance
# saturation - number of protests in a 40km radius in the past 150 days
# west - state in: "Brandenburg", "Sachsen", "Sachsen-Anhalt", "Mecklenburg-Vorpommern", "Thürigen"
# weekend - day of event: Saturday/Sunday


# 6
names(all_data) %>% str_subset("\\.6")
# title
# date
# poldem_part - number of participants according to PolDem
# name
# geocode
# latitude
# longitude
# match
# govt_Ort - location name in govt data
# poldem_saturation - Number of events in a given period (+/-50 days) in a given radius (40km)
# west - state in: "Brandenburg", "Sachsen", "Sachsen-Anhalt", "Mecklenburg-Vorpommern", "Thürigen"
# weekend - day of event: Saturday/Sunday

# poldem30
names(all_data) %>% str_subset("\\.30")
# community - see codebook for PolDem - Protest 30
# id_event- see codebook for PolDem - Protest 30
# doc_source- see codebook for PolDem - Protest 30
# event_n- see codebook for PolDem - Protest 30
# date
# poldem_part - number of participants according to PolDem
# name
# geocode
# latitude
# longitude
# match
# govt_Ort - location name in govt data
# poldem_saturation - Number of events in a given period (+/-50 days) in a given radius (40km)
# west - state in: "Brandenburg", "Sachsen", "Sachsen-Anhalt", "Mecklenburg-Vorpommern", "Thürigen"
# weekend - day of event: Saturday/Sunday

###############
# Models
###############

###############
# poldem6 coverage of govt data
###############

# Subset to poldem6 timeframe
poldem6_sample <-  all_data[all_data$Datum.gvt >= (min(poldem6$date.6) - 3) & all_data$Datum.gvt <= (max(poldem6$date.6)), ]

# Prep vars for analysis
poldem6_sample$west.gvt <- ifelse(poldem6_sample$west.gvt, "west", "east")
poldem6_sample$weekend.gvt <- ifelse(poldem6_sample$weekend.gvt, "yes", "no")
table(poldem6_sample$weekend.gvt)
poldem6_sample$group.gvt <- factor(poldem6_sample$group.gvt)
poldem6_sample$year.gvt <- substr(poldem6_sample$Datum.gvt, 1, 4)  %>% factor
poldem6_sample$group.gvt <- factor(poldem6_sample$group.gvt, levels = c("other", "Neonazis", "NPD"))
poldem6_sample$participants.gvt_100 <- poldem6_sample$participants.gvt / 100

#
GovtPoldem6_1 <- glm(poldem6 ~ participants.gvt_100 + I(participants.gvt_100^2), family = "binomial", poldem6_sample)
summary(GovtPoldem6_1)

#
GovtPoldem6_2 <- glm(poldem6 ~ participants.gvt_100 + I(participants.gvt_100^2) + west.gvt + weekend.gvt, family = "binomial", poldem6_sample)
summary(GovtPoldem6_2)

#
GovtPoldem6_3 <- glm(poldem6 ~ participants.gvt_100 + I(participants.gvt_100^2) + west.gvt + weekend.gvt + saturation.gvt, family = "binomial", poldem6_sample)
summary(GovtPoldem6_3)

#
GovtPoldem6_4 <- glm(poldem6 ~ participants.gvt_100 + I(participants.gvt_100^2) + west.gvt + weekend.gvt + saturation.gvt + group.gvt, family = "binomial", poldem6_sample)
summary(GovtPoldem6_4)

# Cplots
pdf("Latex/figure/GovtPoldem6_cplot.pdf", bg = "white")
par(mfrow = c(2,2))
cplot(GovtPoldem6_1, "participants.gvt_100", xlim = c(0.5, 8), xlab = "Participants (in 100)", ylab = "Predicted probability", ylim = c(0, .4), sub = "(Model 1)", family = "LM Roman 10")
cplot(GovtPoldem6_2, "west.gvt", xlab = "West Germany", ylab = "Predicted probability", ylim = c(0, .4), sub = "(Model 2)", family = "LM Roman 10")
cplot(GovtPoldem6_3, "saturation.gvt", xlab = "Saturation (-  150d, r < 40km)", ylab = "Predicted probability", ylim = c(0, .4), sub = "(Model 3)", family = "LM Roman 10")
cplot(GovtPoldem6_4, "group.gvt", xlab = "Group", ylab = "Predicted probability", ylim = c(0, .4), sub = "(Model 4)", family = "LM Roman 10")
dev.off()


###############
# poldem30 coverage of govt data
###############

# Subset to poldem30 timeframe
poldem30_sample <-  all_data[all_data$Datum.gvt >= (min(poldem30$date.30) - 3) & all_data$Datum.gvt <= (max(poldem30$date.30)), ]

# Prep vars for analysis
poldem30_sample$west.gvt <- ifelse(poldem30_sample$west.gvt, "west", "east")
poldem30_sample$weekend.gvt <- ifelse(poldem30_sample$weekend.gvt, "yes", "no")
table(poldem30_sample$weekend.gvt)
poldem30_sample$group.gvt <- factor(poldem30_sample$group.gvt)
poldem30_sample$year.gvt <- substr(poldem30_sample$Datum.gvt, 1, 4) %>% factor
poldem30_sample$group.gvt <- factor(poldem30_sample$group.gvt, levels = c("other", "Neonazis", "NPD", "PEGIDA and Co.", "Die RECHTE/Der 3. Weg/PRO"))
poldem30_sample$participants.gvt_100 <- poldem30_sample$participants.gvt / 100

#
GovtPoldem30_1 <- glm(poldem30 ~ participants.gvt_100 + I(participants.gvt_100^2), family = "binomial", poldem30_sample)
summary(GovtPoldem30_1)

#
GovtPoldem30_2 <- glm(poldem30 ~ participants.gvt_100 + I(participants.gvt_100^2) + west.gvt + weekend.gvt, family = "binomial", poldem30_sample)
summary(GovtPoldem30_2)

#
GovtPoldem30_3 <- glm(poldem30 ~ participants.gvt_100 + I(participants.gvt_100^2) + west.gvt + weekend.gvt + saturation.gvt, family = "binomial", poldem30_sample)
summary(GovtPoldem30_3)

#
GovtPoldem30_4 <- glm(poldem30 ~ participants.gvt_100 + I(participants.gvt_100^2) + west.gvt + weekend.gvt + saturation.gvt + group.gvt, family = "binomial", poldem30_sample)
summary(GovtPoldem30_4)


# Cplots
pdf("Latex/figure/GovtPoldem30_cplot.pdf", bg = "white")
par(mfrow = c(2,2))
cplot(GovtPoldem30_1, "participants.gvt_100", xlim = c(0.5, 8), xlab = "Participants (in 100)", ylab = "Predicted probability", ylim = c(0, .25), sub = "(Model 1)", family = "LM Roman 10")
cplot(GovtPoldem30_2, "west.gvt", xlab = "West Germany", ylab = "Predicted probability", ylim = c(0, .25), sub = "(Model 2)", family = "LM Roman 10")
cplot(GovtPoldem30_3, "saturation.gvt", xlab = "Saturation (-  150d, r < 40km)", ylab = "Predicted probability", ylim = c(0, .25), sub = "(Model 3)", family = "LM Roman 10")

poldem30_sample$group.gvt_alt <- as.character(poldem30_sample$group.gvt)
poldem30_sample$group.gvt_alt[poldem30_sample$group.gvt_alt  == "Die RECHTE/Der 3. Weg/PRO"] <- "RECHTE/\n3. Weg"
poldem30_sample$group.gvt_alt[poldem30_sample$group.gvt_alt  == "PEGIDA and Co."] <- "PEGIDA"
poldem30_sample$group.gvt_alt <- factor(poldem30_sample$group.gvt_alt, levels = c("other", "Neonazis", "NPD", "PEGIDA", "RECHTE/\n3. Weg"))
GovtPoldem30_4_alt <- glm(poldem30 ~ participants.gvt + I(participants.gvt^2) + west.gvt + saturation.gvt + group.gvt_alt, family = "binomial", poldem30_sample)

cplot(GovtPoldem30_4_alt, "group.gvt_alt", xlab = "Group", ylab = "Predicted probability", ylim = c(0, .25), sub = "(Model 4)", las = 2, family = "LM Roman 10")

dev.off()

###############
# govt coverage of poldem6 data
###############

# Add west/east
all_data$west.6 <- ifelse(all_data$west.6, "yes", "no")
table(all_data$west.6)
all_data$poldem_part.6_100 <- all_data$poldem_part.6 / 100

# Adjust form.6 for visualization
all_data$form.6[all_data$form.6 == "demonstration, protest march"] <- "demo."
all_data$form.6[all_data$form.6 == "demonstration with violence"] <- "w/ violence"
all_data$form.6[all_data$form.6 == "illegal demonstration"] <- "illegal demo."

#
Poldem6Govt_1 <- glm(gvt ~ poldem_part.6_100 + I(poldem_part.6_100^2), family = "binomial", all_data)
summary(Poldem6Govt_1)

#
Poldem6Govt_2 <- glm(gvt ~ poldem_part.6_100 + I(poldem_part.6_100^2) + west.6, family = "binomial", all_data )
summary(Poldem6Govt_2)

#
Poldem6Govt_3 <- glm(gvt ~ poldem_part.6_100 + I(poldem_part.6_100^2) + west.6 + form.6, family = "binomial", all_data )
summary(Poldem6Govt_3)

#
Poldem6Govt_4 <- glm(gvt ~ poldem_part.6_100 + I(poldem_part.6_100^2) + west.6 + form_recode.6, family = "binomial", all_data )
summary(Poldem6Govt_4)


# Cplots
pdf("Latex/figure/Poldem6Govt_cplot.pdf", bg = "white")
par(mfrow = c(2,2))
cplot(Poldem6Govt_1, "poldem_part.6_100", xlim = c(0.5, 8), xlab = "Participants (in 100)", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Model 1)", family = "LM Roman 10")
cplot(Poldem6Govt_2, "west.6", xlab = "West Germany", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Model 2)", family = "LM Roman 10")
cplot(Poldem6Govt_3, "form.6", xlab = "Protest form", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Model 3)", family = "LM Roman 10")
cplot(Poldem6Govt_4, "form_recode.6", xlab = "Protest category", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Model 4)", family = "LM Roman 10")
dev.off()

###############
# govt coverage of poldem30 data
###############

# Add west/east
all_data$west.30 <- ifelse(all_data$west.30, "yes", "no")
table(all_data$west.30)
all_data$weekend.30 <- ifelse(all_data$weekend.30, "yes", "no")
table(all_data$weekend.30)
all_data$poldem_part.30_100 <- all_data$poldem_part.30 / 100

#
Poldem30Govt_1 <- glm(gvt ~ poldem_part.30_100 + I(poldem_part.30_100^2), family = "binomial", all_data)
summary(Poldem30Govt_1)

#
Poldem30Govt_2 <- glm(gvt ~ poldem_part.30_100 + I(poldem_part.30_100^2) + west.30 + weekend.30, family = "binomial", all_data )
summary(Poldem30Govt_2)

#
Poldem30Govt_3 <- glm(gvt ~ poldem_part.30_100 + I(poldem_part.30_100^2) + west.30 + weekend.30 + doc_probability.30, family = "binomial", all_data )
summary(Poldem30Govt_3)

#
Poldem30Govt_4 <- glm(gvt ~ poldem_part.30_100 + I(poldem_part.30_100^2) + west.30 + weekend.30 + doc_probability.30 + radical_action.30, family = "binomial", all_data )
summary(Poldem30Govt_4)

# Cplots
pdf("Latex/figure/Poldem30Govt_cplot.pdf", bg = "white")
par(mfrow = c(2,2))
cplot(Poldem30Govt_1, "poldem_part.30_100", xlim = c(0.5, 8), xlab = "Participants (in 100)", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Model 1)", family = "LM Roman 10")
cplot(Poldem30Govt_2, "west.30", xlab = "West Germany", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Model 2)", family = "LM Roman 10")
cplot(Poldem30Govt_3, "doc_probability.30", xlab = "Document probability", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Model 3)", family = "LM Roman 10")
cplot(Poldem30Govt_4, "radical_action.30", xlab = "Violent/radical action", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Model 4)", family = "LM Roman 10")
dev.off()

#####################
# Cplots for participants
#####################

pdf("Latex/figure/participants_cplot.pdf", bg = "white")
par(mfrow = c(2,2))
cplot(GovtPoldem30_1, "participants.gvt_100", xlim = c(0.5, 8), xlab = "Participants (in 100)", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Government in semi-  supervised)", family = "LM Roman 10")
cplot(Poldem30Govt_1, "poldem_part.30_100", xlim = c(0.5, 8), xlab = "Participants (in 100)", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Semi-  supervised in government)", family = "LM Roman 10")

cplot(GovtPoldem6_1, "participants.gvt_100", xlim = c(0.5, 8), xlab = "Participants (in 100)", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Government in handcoded)", family = "LM Roman 10")
cplot(Poldem6Govt_1, "poldem_part.6_100", xlim = c(0.5, 8), xlab = "Participants (in 100)", ylab = "Predicted probability", ylim = c(0, 1), sub = "(Handcoded in government)", family = "LM Roman 10")
dev.off()


###############
# Stargazer
###############


stargazer(GovtPoldem6_1, GovtPoldem6_2, GovtPoldem6_3, GovtPoldem6_4, #type = "text", 
          title = "Results from logistic regression (Government in handcoded)",
          covariate.labels= c("Participants (in 100)", "Participants\\textsuperscript{2}", "West Germany", "Weekend", "Saturation ($-$150d, r < 40km)", "Group: Neonazis", "Group: NPD"),
          dep.var.labels = "Government event covered by handcoded",
          dep.var.caption = "Dependent variable:",
          label = "tab:GovtPoldem6",
          table.placement	= "!ht",
          out = "Latex/table/GovtPoldem6.tex")

stargazer(GovtPoldem30_1, GovtPoldem30_2, GovtPoldem30_3, GovtPoldem30_4, #type = "text",
          title = "Results from logistic regression (Government in semi-supervised)",
          covariate.labels= c("Participants (in 100)", "Participants\\textsuperscript{2}", "West Germany", "Weekend", "Saturation ($-$150d, r < 40km)", "Group: Neonazis (ref: other)", "Group: NPD", "Group: PEGIDA and Co.", "Group: Die RECHTE/Der 3. Weg/PRO"),
          dep.var.labels = "Government event covered by semi-supervised",
          dep.var.caption = "Dependent variable:",
          label = "tab:GovtPoldem30",
          table.placement	= "!ht",
          out = "Latex/table/GovtPoldem30.tex")

stargazer(Poldem6Govt_1, Poldem6Govt_2, Poldem6Govt_3, Poldem6Govt_4, #type = "text",
          title = "Results from logistic regression (Handcoded in government)",
          covariate.labels= c("Participants (in 100)", "Participants\\textsuperscript{2}", "West Germany", "Protest form: Demo. (ref: Demo. w/ violence)", "Protest form: Illegal Demo.", "Protest category: demonstrative (ref: confrontational)", "Protest category: light violence"),
          dep.var.labels = "Handcoded event covered by government data",
          dep.var.caption = "Dependent variable:",
          label = "tab:Poldem6Govt",
          table.placement	= "!ht",
          out = "Latex/table/Poldem6Govt.tex")

stargazer(Poldem30Govt_1, Poldem30Govt_2, Poldem30Govt_3, Poldem30Govt_4, #type = "text",
          title = "Results from logistic regression (Semi-supervised in government)",
          covariate.labels= c("Participants (in 100)", "Participants\\textsuperscript{2}", "West Germany", "Weekend", "Document probability", "Violent/radical action"),
          dep.var.labels = "Semi-supervised event covered by government data",
          dep.var.caption = "Dependent variable:",
          label = "tab:Poldem30Govt",
          table.placement	= "!ht",
          out = "Latex/table/Poldem30Govt.tex")

