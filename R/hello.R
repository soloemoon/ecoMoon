extrafont::loadfonts(device="win")
if (!require("fredr")) {install.packages("fredr"); library("fredr")}
if (!require("purrr")) {install.packages("purrr"); library("purrr")}
if (!require("reshape2")) {install.packages("reshape2"); library("reshape2")}
if (!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if (!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
if (!require("zoo")) {install.packages("zoo"); library("zoo")}
if (!require("ggplot2")) {install.packages("ggplot2"); library("ggplot2")}
if (!require("scales")) {install.packages("scales"); library("scales")}
if (!require("tis")) {install.packages("tis"); library("tis")}
if (!require("gridExtra")) {install.packages("gridExtra"); library("gridExtra")}
if (!require("grid")) {install.packages("grid"); library("grid")}
if (!require("mFilter")) {install.packages("mFilter"); library("mFilter")}
if (!require("formattable")) {install.packages("formattable"); library("formattable")}
if (!require("forecast")) {install.packages("forecast"); library("forecast")}
if (!require("pdfetch")) {install.packages("pdfetch"); library("pdfetch")}
if (!require("xts")) {install.packages("xts"); library("xts")}
if (!require("ggtheme")) {install.packages("ggtheme"); library("ggtheme")}

APIKey <-function(key){fredr_set_key(key)}
EcoDataMonthly <- function(fred.key,col.rename){
  FredrNames <-append('USRECM', fred.key)
  df <-data.frame(map_dfr(fred.key,fredr))
  df <-reshape(df, idvar='date', timevar='series_id', direction='wide')
  col.rename <-append('Recession', col.rename)
  col.rename <-append('date', col.rename)
  colnames(df) <-col.rename
  df <-df[order(as.Date(df[,1], format="%d/%m/%Y")),]
  rownames(df) <-df$date
  df$Quarter <-paste(quarters(as.Date(df[,1])), format(df[,1], '%y'), sep=" ")
  return(df)
}

