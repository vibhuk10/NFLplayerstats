library(tidyverse)
library(rvest)

# get variable from df
getVar <- function(df, row, column) {
  val <- df[row,column]
  val <- val[[1]]
  val
}

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

# PFR scraping
scrapePFRData = function(urlend, startyr, endyr, numTable) {
  df = data.frame()
  for (i in startyr:endyr) {
    URL = paste('https://www.pro-football-reference.com/years/', as.character(i), urlend, sep = "")
    table = html_table(html_nodes(read_html(URL),"table")[[numTable]])
    #correct column names
    for(a in 1:ncol(table)) {
      if(colnames(table)[a] ==  "") {
        colnames(table) <- as.character(table[1,])
        table <- table[-c(1),]
      }
    }
    #remove extra column names
    extraRowNums <- c()
    for(b in 1:nrow(table)) {
      if(colnames(table)[2] == getVar(table,b,2)) {
        extraRowNums <- append(extraRowNums, b)
      }
    }
    table <- table[-c(extraRowNums),]
    #remove asterisk in the player name
    for(c in 1:ncol(table)) {
      if(colnames(table)[c] ==  "Player") {
        for(d in 1:nrow(table)) {
          if(grepl("*", getVar(table, d, c), fixed = TRUE)) {
            table[d,c] = gsub("*","", getVar(table, d, c), fixed = TRUE)
          }
          table[d,c] = trimws(getVar(table, d, c))
        }
      }
    }
    # reset the row numbers
    rownames(table) <- seq(length=nrow(table))
    table$Year = i
    df <- rbind(table, df)
  }
  df
}

# tibble for each data set that is scraped through scrapePFRData
PFRDataNames <- 
  tibble(
    varName = c('stanPass_20','stanRush_20','stanRecv_20', 'advPassAY_20', 'advPassAcc_20'),
    urlend = c('/passing.htm', '/rushing.htm', '/receiving.htm', '/passing_advanced.htm', '/passing_advanced.htm'),
    startyr = c(2020, 2020, 2020, 2020, 2020),
    endyr = c(2020, 2020, 2020, 2020, 2020),
    numTable = c(1, 1, 1, 1, 2)
  )

# for loop to scrape each dataset in PFRDataNames through ScrapePFRData
for(i in 1:nrow(PFRDataNames)) {
  assign(getVar(PFRDataNames, i, 1), scrapePFRData(getVar(PFRDataNames, i, 2), getVar(PFRDataNames, i, 3), getVar(PFRDataNames, i, 4), getVar(PFRDataNames, i, 5)))
}

# clean PFR data for downloaded files
cleanPFRData = function(df, year) {
  #correct column names
  if(colnames(df)[1] ==  "X1") {
    colnames(df) <- as.character(df[1,])
    df <- df[-c(1),]
  }
  #remove asterisk in the player name
  for(c in 1:ncol(df)) {
    if(colnames(df)[c] ==  "Player") {
      for(d in 1:nrow(df)) {
        if(grepl("*", getVar(df, d, c), fixed = TRUE)) {
          df[d,c] = gsub("*","", getVar(df, d, c), fixed = TRUE)
        }
        df[d,c] = trimws(getVar(df, d, c))
      }
    }
  }
  #add the year
  df$Year = year
  df
}

# advPassAcc_20 <- read_csv("data-raw/advPassAcc_2020.csv")
# advPassAcc_20 <- cleanPFRData(advPassAcc_20, 2020)