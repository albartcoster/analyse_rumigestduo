library(openxlsx)
library(dplyr)
library(echarts4r)
library(lubridate)
library(shiny)
library(bslib)
library(tidyr)

input <- choose.files(filters=c("xlsx"),multi=TRUE)

cns <- c("id",
         "datetime",
         "aname",
          "birth_date",
          "group",
          "reg_id",
          "dim",
          "production",
          "isk",
          "fat_pct",
          "protein_pct",
          "lact_pct",
          "lactation",
          "rest1",
          "rest2",
          "rest3",
          "feed1",
          "feed2",
          "feed3",
          "mikings",
          "rejections")

df <- data.frame()
for(i in input){
  df1 <- read.xlsx(i,sheet = 1,detectDates = T)
  colnames(df1) <- cns
  df <- rbind(df,df1)
}
df <- unique(df)
df <- df |> 
  mutate(across((7:21),as.numeric)) |> 
  mutate(
    nlactcat = cut(lactation,c(0,1,2,100),c("1 lakt","2 lakts","3+ lakts.")),
    dilcat = cut(dim,c(0,30,60,200,300,2000),c('0-30 DIL','30-60 DIL',"60-200 DIL", "200-300 DIL","300+DIL"))
  ) |> 
  filter(!is.na(production)&production>0) |> 
  mutate(month = floor_date(datetime,"week"))
save(df,file ="../data.Rda")

