#plumber.R
Sys.setlocale("LC_ALL","English")
library(plumber)
library(mongolite)
library(tidyr)
library(dplyr)
library(chron)
library(purrr)
library(stringr)
library(lubridate)
library(plotly)

mongo_url <- "mongodb+srv://eric2mballus:tititata85@cluster0.e1pkdu7.mongodb.net/swivy_db"

list_tables <- c("activitieitems", "activities", "categories", "channelclusters", "companies", "pos", "road_items",
                 "roads", "salereps")

fetch_data <- function(){
  dataframes <- list()
  
  # Boucle sur les collections
  for (collection_name in list_tables) {
    # Connexion à la collection
    collection_conn <- mongo(collection = collection_name, db = "swivy_db", url = mongo_url)
    
    # Lire les données et les transformer en dataframe
    data <- collection_conn$find(field = '{}')
    
    # Ajouter le dataframe à la liste
    dataframes[[collection_name]] <- data
  }
  
  return(dataframes)
  
}

data_gauge <- function(){
  for (table_name in list_tables) {
    assign(table_name, fetch_data()[[table_name]])
  }
  road_items <<- road_items
  activitieitems <<- activitieitems
  
  roadsitems <- merge(road_items, roads,by.x = "roadId",by.y="_id",all.x = TRUE)
  roadsitems <- roadsitems %>% select(roadId,`_id`,posId,
                                      id_company.x, saleRep, roadItems,status.x,execution_date) %>%
    rename(id_road_items=`_id`,id_company=id_company.x,status=status.x)
  
  #join the precedent dataframe with salereps
  roadsitems_srep <- merge(roadsitems,salereps,by.x = "saleRep", by.y = "_id",all.x = TRUE) 
  roadsitems_srep <- roadsitems_srep %>% select(roadId, id_road_items,roadItems,saleRep,id_company.x,status.x,execution_date,name,posId)
  
  #join the precedent dataframe with pos
  roadsitems_srep_pos <- merge(roadsitems_srep,pos,by.x="posId", by.y="_id",all.x = TRUE)  
  roadsitems_srep_pos <- roadsitems_srep_pos %>% select(roadId,id_road_items,roadItems,saleRep,id_company.x,status.x,execution_date,name.x,name.y,posId,
                                                        city,channelCluster) %>%
    rename(name_salerep=name.x,name_pos=name.y)
  
  #join the precedent dataframe with channelclusters
  roadsitems_srep_pos_chc <- merge(roadsitems_srep_pos,channelclusters,by.x="channelCluster",by.y = "_id",all.x = TRUE)
  roadsitems_srep_pos_chc <<- roadsitems_srep_pos_chc %>% select(roadId,id_road_items,roadItems,saleRep,id_company,status.x,execution_date,name_salerep,posId,name_pos,
                                                                city,channelCluster,name) %>% 
    rename(name_cluster=name)
  
  #rename the modalities of variable city
  roadsitems_srep_pos_chc$city <<- gsub("Yaoundé, Cameroon","Yaounde",roadsitems_srep_pos_chc$city)
  return(roadsitems_srep_pos_chc)
}

#* Get data for the gauge chart graphic
#* @get /data_store_visited

function(salerep=NULL, idpos=NULL,idcluster=NULL,location=NULL,timestart=NULL,timeend=NULL){ #,posId,saleRep,channelCluster
  filtre <-data_gauge()
  if (!is.null(salerep)){
     filtre <- filtre %>% filter(saleRep==salerep)
  }
  if (!is.null(idpos)){
     filtre <- filtre %>% filter(posId==idpos)
  }
  if (!is.null(idcluster)) {
    filtre <- filtre %>% filter(channelCluster==idcluster)
  }
  if (!is.null(location)) {
    filtre <- filtre %>% filter(city==location)
  }
  if (!is.null(timestart) & !is.null(timeend)) {
    filtre <- filtre %>% filter(execution_date>=timestart & execution_date<=timeend)
  }
  return (filtre)
}

data_table <- function(){
  
  flatten_taskid <- function(df) {
    df %>%
      mutate(taskid = map(taskIds, as_tibble)) %>%
      unnest(taskid, names_sep = "_")
  }
  
  data <- data_gauge()
  data_table1 <- merge(data,road_items,by.x = "id_road_items",by.y = "_id")
  data_table1$id_company.x <- NULL
  data_table2 <- flatten_taskid(data_table1)
  
  data_table2 <- data_table2 %>%
    mutate(starttime=str_extract(taskid_startTime, "\\d{2}:\\d{2}:\\d{2}")) %>%
    mutate(endtime=str_extract(taskid_endTime, "\\d{2}:\\d{2}:\\d{2}")) 
  data_table2$starttime <- times(data_table2$starttime)
  data_table2$endtime <- times(data_table2$endtime)
  # data_table2$gap <- data_table2$endtime -data_table2$starttime
  # data_table2$gap <- round(data_table2$gap * 1440)
  
  data_table2 <- data_table2 %>%
    mutate(date_start=str_extract(taskid_startTime, "\\w{3} \\d{2} \\d{4}")) %>%
    mutate(date_end=str_extract(taskid_endTime, "\\w{3} \\d{2} \\d{4}"))
  data_table2$date_start <- as.Date(data_table2$date_start, format = "%b %d %Y")
  data_table2$date_end <- as.Date(data_table2$date_end, format = "%b %d %Y")
  
  data_table2$st <- paste(data_table2$date_start," ",data_table2$starttime)
  data_table2$et <- paste(data_table2$date_end," ",data_table2$endtime)
  data_table2$st <- as.POSIXct(data_table2$st,format = "%Y-%m-%d   %H:%M:%S")
  data_table2$et <- as.POSIXct(data_table2$et,format = "%Y-%m-%d   %H:%M:%S")
  data_table2$time_performed <- round(difftime(data_table2$et, data_table2$st, units = "mins"))
  data_table2$time_performed <- as.numeric(data_table2$time_performed)

  #taskactivityitem et idactivityitem
  final_df <- merge(data_table2,activitieitems,by.x = "taskid_activityItem", by.y="_id")
  final_df$status.x <- NULL
  final_df$starttime <- as.character(final_df$starttime)
  final_df$endtime <- as.character(final_df$endtime)
  return(final_df)
}

#* Get data for the table of sales representant
#* @get /data_sales_rep
function(salerep=NULL, idpos=NULL,idcluster=NULL,location=NULL,timestart=NULL,timeend=NULL) {
  filtre <-data_table()
  if (!is.null(salerep)){
    filtre <- filtre %>% filter(saleRep==salerep)
  }
  if (!is.null(idpos)){
    filtre <- filtre %>% filter(posId==idpos)
  }
  if (!is.null(idcluster)) {
    filtre <- filtre %>% filter(channelCluster==idcluster)
  }
  if (!is.null(location)) {
    filtre <- filtre %>% filter(city==location)
  }
  if (!is.null(timestart) & !is.null(timeend)) {
    filtre <- filtre %>% filter(execution_date>=timestart & execution_date<=timeend)
  }
  return (filtre)
}

#* Get data for the bar chart graphic of pos visited by channel cluster
#* @get /data_store_per_channelcluster

function(salerep=NULL, idpos=NULL,idcluster=NULL,location=NULL,timestart=NULL,timeend=NULL) { 
  filtre <- data_gauge()
  
  if (!is.null(salerep)){
    filtre <- filtre %>% filter(saleRep==salerep)
  }
  if (!is.null(idpos)){
    filtre <- filtre %>% filter(posId==idpos)
  }
  if (!is.null(idcluster)) {
    filtre <- filtre %>% filter(channelCluster==idcluster)
  }
  if (!is.null(location)) {
    filtre <- filtre %>% filter(city==location)
  }
  if (!is.null(timestart) & !is.null(timeend)) {
    filtre <- filtre %>% filter(execution_date>=timestart & execution_date<=timeend)
  }
  
  dataf <- filtre %>% filter(status.x=="completed")
  dataf <- as.data.frame(table(dataf$name))
  return (dataf)
}
