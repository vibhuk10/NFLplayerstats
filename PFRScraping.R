library(tidyverse)
library(rvest)

# general scarping
scrapeData = function(urlprefix, urlend, startyr, endyr) {
  master = data.frame()
  for (i in startyr:endyr) {
    cat('Loading Year', i, '\n')
    URL = paste(urlprefix, as.character(i), urlend, sep = "")
    table = html_table(html_nodes(read_html(URL),"table")[[1]])
    table$Year = i
    master = rbind(table, master)
  }
  return(master)
}

# PFR scarping
scrapePFRData = function(urlend, startyr, endyr) {
  df = data.frame()
  for (i in startyr:endyr) {
    cat('Loading Year', i, '\n')
    URL = paste('https://www.pro-football-reference.com/years/', as.character(i), urlend, sep = "")
    table = html_table(html_nodes(read_html(URL),"table")[[1]])
    table$Year = i
    df = rbind(table, df)
  }
  df
}

# clean up data
cleanPFRData = function(df) {
  colnames(df) <- as.character(df[1,])
  df <- df[-c(1),]
  df
}

standard_passing_20 <- scrapePFRData('/passing.htm', 2020, 2020)
standard_rushing_20 <- scrapePFRData('/rushing.htm', 2020, 2020) %>% cleanPFRData()

#standard_passing_20 <- scrapeData('https://www.pro-football-reference.com/years/', '/passing.htm', 2020, 2020)