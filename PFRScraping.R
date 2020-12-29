library(tidyverse)
library(rvest)

# general scarping
scrapeData = function(urlprefix, urlend, startyr, endyr) {
  master = data.frame()
  for (i in startyr:endyr) {
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
    URL = paste('https://www.pro-football-reference.com/years/', as.character(i), urlend, sep = "")
    table = html_table(html_nodes(read_html(URL),"table")[[1]])
    #clean data
    for(a in ncol(table)) {
      if(colnames(table)[a] ==  "") {
        colnames(table) <- as.character(table[1,])
        table <- table[-c(1),]
      }
    }
    table$Year = i
    df = rbind(table, df)
  }
  df
}


# get variable from df
getVar <- function(df, row, column) {
  val <- df[row,column]
  val <- val[[1]]
  val
}

PFRDataNames <- 
  tibble(
    varName = c('standard_passing_20','standard_rushing_20'),
    urlend = c('/passing.htm', '/rushing.htm'),
    startyr = c(2020, 2020),
    endyr = c(2020, 2020),
    clean = c(0, 1)
  )

for(i in 1:nrow(PFRDataNames)) {
  assign(getVar(PFRDataNames, i, 1), scrapePFRData(getVar(PFRDataNames, i, 2), getVar(PFRDataNames, i, 3), getVar(PFRDataNames, i, 4)))
}

#standard_passing_20 <- scrapePFRData('/passing.htm', 2020, 2020)
#standard_rushing_20 <- scrapePFRData('/rushing.htm', 2020, 2020)
#standard_passing_20 <- scrapeData('https://www.pro-football-reference.com/years/', '/passing.htm', 2020, 2020)