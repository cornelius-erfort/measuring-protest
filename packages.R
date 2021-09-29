p_needed <- c("stringr", "readxl", "dplyr", "tidyr", "plyr", "httr", "rvest", "urltools", "XML", "gnumeric", "english", "gsubfn",  "lubridate", "pdftools", "tabulizer", "ggplot2", "writexl", "ggmap", "sp", "rgdal", "stargazer", "geodist", "lemon", "gridExtra", "quanteda", "margins", "extrafont", "bibtex")
p_install <- p_needed[!(p_needed %in% rownames(installed.packages()))]
if (length(p_install) > 0) install.packages(p_install)
lapply(p_needed, require, character.only = TRUE)
rm("p_needed", "p_install")

loadfonts()
