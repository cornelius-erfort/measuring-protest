#### Term Paper QTA
## Summer Semester 2020
# Constructing the govt dataset "Kleine Anfragen - Bundestag"

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

#########################################################################################

make.folder("data.nosync/Anfragen")

################
# Download index pages
################

# Kleine Anfragen Bundestag, manually downloaded two index pages: https://pdok.bundestag.de/treffer.php?q=%22rechtsextreme%20aufm%C3%A4rsche%22&wp=&dart=&gtyp=&typ=&gkuname=&kurheber=&gpuname=&purheber=, https://pdok.bundestag.de/treffer.php?h=100&q=%22rechtsextreme+aufm%C3%A4rsche%22

# Extract list of files and info
anfragen_html <- read_html("data.nosync/Anfragen/Anfragen_index1.html")
anfragen <- data.frame(title = html_nodes(anfragen_html, "div.linkGeneric") %>% html_text(), link = html_nodes(anfragen_html, "div.linkGeneric") %>% html_nodes("a") %>% html_attr("href"), info = html_nodes(anfragen_html, "tr")[-1] %>% html_text())
anfragen_html <- read_html("data.nosync/Anfragen/Anfragen_index2.html")
anfragen <- rbind(anfragen, data.frame(title = html_nodes(anfragen_html, "div.linkGeneric") %>% html_text(), link = html_nodes(anfragen_html, "div.linkGeneric") %>% html_nodes("a") %>% html_attr("href"), info = html_nodes(anfragen_html, "tr")[-1] %>% html_text()))

# Subset to answers only, to protest info only, exclude special answers
anfragen <- anfragen[str_detect(anfragen$info, "Urheber: Bundesregierung") & str_detect(anfragen$title, "Rechtsextreme Aufmärsche") & !str_detect(anfragen$title, "(Dresden)|(Arbeit)|(Nachfrage)"), ]

# Extract date info
anfragen <- cbind(anfragen, data.frame(quarter = str_replace_all(anfragen$title, c("ersten" = "1", "1\\." = "1", "zweiten" = "2", "2." = "2 ", "dritten" = "3", "vierten" = "4")) %>% str_extract("[:digit:](?= Quartal)"), year = str_extract(anfragen$title, "(?<=Quartal )[:digit:]{4}")))
table(anfragen$year)
table(anfragen$quarter)


################
# Download pdfs
################

make.folder("data.nosync/Anfragen/pdf")
anfragen$downloaded <- NA
for (i in 1:nrow(anfragen)) {
  path <- str_c("data.nosync/Anfragen/pdf/", basename(anfragen$link[i]))
  if((i %% 5) == 0) print((i/nrow(anfragen)*100) %>% round(1)) # Output status
  if(!file.exists(path)) {
    download.file(anfragen$link[i], destfile = path, quiet = T, headers = header)
    Sys.sleep(runif(1, .2, .3))
  }
  anfragen$downloaded[i] <- Sys.Date()
  if((i %% 10) == 0) save(anfragen, file = "data.nosync/Anfragen/anfragen.RData")
}
save(anfragen, file = "data.nosync/Anfragen/anfragen.RData")

# save(anfragen, file = "data.nosync/Anfragen/anfragen_final.RData")

################
# Create BibTex entries (copy to clipboard and import to zotero)
################


anfragen$bib_title <- anfragen$info %>% str_replace_all(c("\n" = " ", "\t" = " ")) %>% str_extract("(Antwort {1,5}der.*?(?=(  V ?o r)))|(Antwort {1,5}der.*?(Quartal 20[:digit:]{2}))") %>% str_remove("^.*\\.\\.\\.") %>% str_replace_all("  ", " ") %>% str_replace_all("  ", " ") %>% str_replace_all("  ", " ") %>% str_remove("Die Antwort wurde .*den Fragetext") %>% str_remove("((^\\. )|(^Deutscher)).*Wahlperiode [:digit:]{2}\\. ?[:digit:]{2}\\. ?[:digit:]{4} ") %>% str_replace_all(c("g˘" = "ğ")) %>% str_trim
anfragen$bib_author <- "Bundesregierung"
anfragen$bib_date <- anfragen$info %>% str_extract("(?<=vom )[:digit:]{2}\\. ?[:digit:]{2}\\. ?[:digit:]{4}") %>% dmy()

anfragen$bib_number <- anfragen$info %>% str_replace_all(c("\n" = " ", "\t" = " ")) %>% str_extract("(?<=\\) )Drucksache.*?(?=vom)")
anfragen$bib_link <- anfragen$link

str_c(anfragen$bib_number %>% str_remove("Drucksache Nr\\.\\:") %>% str_trim %>% sort, collapse = "; ")

for (i in 1:nrow(anfragen)) {
  cat("@techreport{\n")
  
  cat("title = {")
  cat(anfragen$bib_title[i])
  cat("}\n")
  
  cat("url = {")
  cat(anfragen$bib_link[i])
  cat("}\n")
  
  cat("number = {")
  cat(anfragen$bib_number[i])
  cat("}\n")
  
  cat("institution = {Deutscher Bundestag}\n")
  
  cat("author = {Bundesregierung}\n")
  

  cat("date = {")
  cat(anfragen$bib_date[i] %>% as.character())
  cat("}\n")
  
  cat("}\n\n")
}


################
# Extract events from pdfs
################

events <- data.frame()
# for (i in 1:nrow(anfragen)) { # Go through all files
for (i in 1:nrow(anfragen)) { # Go through all files
  
  # Load pdf
  path <- str_c("data.nosync/Anfragen/pdf/", basename(anfragen$link[i]))
  cat("\n\n", basename(anfragen$link[i]))
  tables <- extract_tables(path)
  if(i == 16 | i == 47) next # ADD MANUALLY
  tables <- tables[lapply(tables, function (x) str_detect(x[1, 1], "atum")) %>% unlist %>% which]
  
  for (j in 1:length(tables)) { # Go through tables
    if((i == 12 & j == 1) | (i == 13 & j == 1)) next # ADD MANUALLY
    cat(". ")
    
    # Extract table
    demo_table <- tables[[j]][-1, ]
    if(ncol(as.matrix(demo_table)) == 1) demo_table <- demo_table %>% t
    demo_table <-  as.data.frame(demo_table)
    names(demo_table) <- tables[[j]][1, ]
    
    # Prepare rows for merge (aggreagte by index var)
    demo_table$index <- !(demo_table[, 1] == "")
    demo_table$index[which(demo_table$index)] <- seq_along(demo_table$index[which(demo_table$index)])
    demo_table$index[demo_table$index == 0] <- NA
    
    # Correct parsing errors
    if(str_detect(demo_table[1, 1], " [:upper:]{2}$")) names(demo_table)[2] <- (str_split(names(demo_table)[1], " ") %>% unlist)[3] 
    if(length(names(demo_table)) > 7) if(sum(names(demo_table)[7:8] == c("TN", "")) == 2) {
      demo_table <- demo_table[, -7]
      names(demo_table)[7] <- "TN"
    }
    
    demo_table <- fill(demo_table, index, .direction = "down") # Prepare rows for merge
    
    # Merge rows
    demo_table <- aggregate(. ~ index, demo_table, FUN = function(x) paste0(x, collapse = " "))
    
    # Unitize varnames
    names(demo_table)[str_detect(names(demo_table), "(Teil)|(TN)")] <- "participants"
    names(demo_table)[str_detect(names(demo_table), "(rganisa)|(eranstalt)")] <- "orga"
    names(demo_table)[str_detect(names(demo_table), "(uord)")] <- "Zuordnung"
    
    # Make participants numeric
    demo_table$participants <- demo_table$participants %>% str_remove_all("\\.") %>% as.numeric()
    
    # Correct parsing errors
    demo_table[, 2] <- str_trim(demo_table[, 2])
    if(str_detect(demo_table[1, 2], " [:upper:]{2}$")) {
      demo_table[, (str_split(names(demo_table)[2], " ") %>% unlist)[2]] <- str_extract(demo_table[, 2], "[:upper:]{2}$")
      demo_table[, 2] <- str_remove(demo_table[, 2], " [:upper:]{2}$")
      names(demo_table)[2] <- (str_split(names(demo_table)[2], " ") %>% unlist)[1]
    }
    
    if(length(str_split(names(demo_table)[3], " ") %>% unlist) > 1) {
      demo_table[, (str_split(names(demo_table)[3], " ") %>% unlist)[1]] <- str_extract(demo_table[, 3], "^[:upper:]{2}")
      demo_table[, 3] <- str_remove(demo_table[, 3], "^[:upper:]{2} ")
      names(demo_table)[3] <- (str_split(names(demo_table)[3], " ") %>% unlist)[2]
    }
    
    # Add info
    demo_table$file <- path
    demo_table$table_id <- j
    
    # Bind to events dataset
    events <- rbind.fill(events, demo_table)
  }
}

###########
# Manual coding
###########

# Files to be coded:
str_c("data.nosync/Anfragen/pdf/", basename(anfragen$link[12])) # Only table 1
str_c("data.nosync/Anfragen/pdf/", basename(anfragen$link[13])) # Only table 1
str_c("data.nosync/Anfragen/pdf/", basename(anfragen$link[16]))
str_c("data.nosync/Anfragen/pdf/", basename(anfragen$link[47]))

# Correct date
events$Datum <- events$Datum %>% str_replace_all(c("April" = "4", "Mai" = "5", "Juni" = "6")) %>% str_remove("([:alpha:]| )") %>% str_replace("206", "2006") %>% dmy()

handcode <- read_xlsx("data.nosync/Anfragen/handcoding_anfragen.xlsx")
handcode$Datum <- handcode$Datum %>% str_c() %>% ymd()
events <- rbind.fill(events, handcode)

###########
# Cleaning
###########

# Trim vars
events <- apply(events, MARGIN = 2, str_trim) %>% as.data.frame()

# Clean state and place
events$Land[is.na(events$Land)] <- str_extract(events$Ort[is.na(events$Land)], "(?<=\\()[:upper:]{2}(?=\\))")
events$Ort <- str_remove(events$Ort, "\\([:upper:]{2}\\)")
events$Land <- str_replace_all(events$Land %>% toupper, c("NRW" = "NW", "BR" = "BE", "SA" = "ST"))

# Correct punctuation and clean missings
events <- apply(events, MARGIN = 2, function (x) str_remove_all(x, "(?<=[:alpha:])-( |\r)(?=[:lower:])|(?<=/)( |\r)")) %>% as.data.frame()
events <- apply(events, MARGIN = 2, function (x) ifelse(x %in% c("", "n.b.", "n. b."), NA, x)) %>% as.data.frame()

# Add vars for year, month and day
events <- cbind(events, data.frame(year = events$Datum %>% substr(1, 4) %>% as.numeric, month = events$Datum %>% substr(6, 7) %>% as.numeric, day = events$Datum %>% substr(9, 10) %>% as.numeric))

# Add own event id
events$id <- rownames(events)

# Export and reimport
write_xlsx(events, "data.nosync/Anfragen/events.xlsx")
save(events, file = "data.nosync/Anfragen/events.RData")
events <- read_xlsx("data.nosync/Anfragen/events_edit.xlsx") # Some demonstrations take place at multiple locations: they were duplicated but share the same id

# Make participants numeric
events$participants <- as.numeric(events$participants)

# Fornat date var
events$Datum <- ymd(events$Datum)

# Add state
events$state <- str_replace_all(events$Land, c("BE" = "Berlin", "BB" = "Brandenburg", "BW" = "Baden-Württemberg", "BY" = "Bayern", "ST" = "Sachsen-Anhalt", "TH" = "Thüringen", "NI" = "Niedersachsen", "NW" = "Nordrhein-Westfalen", "RP" = "Rheinland-Pfalz", "SN" = "Sachsen", "MV" = "Mecklenburg-Vorpommern", "SH" = "Schleswig-Holstein", "HH" = "Hamburg", "HB" = "Bremen", "HE" = "Hessen", "SL" = "Saarland"))

# Add west/east
events$west <- ifelse(events$state %in% c("Brandenburg", "Sachsen", "Sachsen-Anhalt", "Mecklenburg-Vorpommern", "Thüringen"), F, T)

# The main location var is now "location"
events$location <- str_replace_all(events$location, "\r", " ") %>% str_replace_all("- ", "-")
events$name <- str_c(events$location, ", ", events$state, ", Germany")

events$spontan <- str_detect(events$Motto %>% tolower, "spontan")
events$spontan[is.na(events$spontan)] <- F

# Identify duplicates and drop
events$is_duplicate <- duplicated(select(events, c("Datum", "name", "participants")), fromLast = T)
events <- events[!events$is_duplicate, ]

# Add var for weekend
events$weekend <- weekdays(events$Datum) %in% c("Saturday", "Sunday")

###########
# Geocoding
###########


load("data.nosync/Anfragen/locations.RData")
locations <- rbind(locations, data.frame(name = unique(events$name)[!(unique(events$name) %in% locations$name)], geocode = NA))

# Geocodes
if(file.exists("data.nosync/Anfragen/locations.RData")) load("data.nosync/Anfragen/locations.RData") else { # If geocode is already stored, then load otherwise:
  locations <- data.frame(name = events$name %>% unique) # Get unique location names
  nrow(locations) # 722 locations
  if(!("geocode" %in% names(location))) locations$geocode <- NA # Add col "geocode" if not already
  for (i in 1:nrow(locations)) if(is.na(locations$geocode[i] )) locations$geocode[i] <- geocode(locations$name[i], source = "google", output = "all") # If not geocoded, then geocode
  if(!file.exists("data.nosync/Anfragen/locations.RData")) save(locations, file = "data.nosync/Anfragen/locations.RData") # Save, but don't overwrite
}

events <- merge(events, locations, by = "name", all.x = T)

events$latitude <- sapply(events$geocode, function (x) x[[1]]$geometry$location$lat)
events$longitude <- sapply(events$geocode, function (x) x[[1]]$geometry$location$lng)

# Construct group var
events$group <- "other"
events$group[str_detect(events$Zuordnung %>% toupper, "(NEONAZI)") | str_detect(events$orga %>% toupper, "(NEONAZI)")] <- "Neonazis"
events$group[str_detect(events$Zuordnung %>% toupper, "(RECHTE)|(III)|(PRO)") | str_detect(events$orga %>% toupper, "(RECHTE)|(III)|(PRO)")] <- "Die RECHTE/Der 3. Weg/PRO"
events$group[str_detect(events$Zuordnung %>% toupper, "(NPD)|(JN)") | str_detect(events$orga %>% toupper, "(NPD)|(JN)")] <- "NPD"
events$group[str_detect(events$orga %>% toupper, "(GIDA)")] <- "PEGIDA and Co."
table(events$group)
events$group <- factor(events$group, c("Neonazis", "NPD", "PEGIDA and Co.", "Die RECHTE/Der 3. Weg/PRO", "other"))

# Number of events in a given period (150 days) in a given radius (40km)
day_max <- 150
dist_max <- 40 * 1000
events$saturation <- 0
for (i in 1:nrow(events)) {
  if((i %% 40) == 0) print(i)
  observation <- events[i, ] # Subset observation
  possibles <- events[(ymd(observation$Datum) - ymd(events$Datum)) <= day_max & (ymd(observation$Datum) - ymd(events$Datum)) >= 1, ] # Find all events in the time frame
  nrow(possibles)
  events$saturation[i] <- nrow(possibles[geodist(x = data.frame(lon = observation$longitude, lat = observation$latitude), y = data.frame(lon = possibles$longitude, lat = possibles$latitude)) <= dist_max, ])
}  

# save(events, file = "data.nosync/Anfragen/events_final.RData")


