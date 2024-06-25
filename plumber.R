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
  roadsitems_srep_pos_chc <- roadsitems_srep_pos_chc %>% select(roadId,id_road_items,roadItems,saleRep,id_company,status.x,execution_date,name_salerep,posId,name_pos,
                                                                city,channelCluster,name,color) %>% 
    rename(name_cluster=name)
  
  #rename the modalities of variable city
  roadsitems_srep_pos_chc$city <- gsub("Yaoundé, Cameroon","Yaounde",roadsitems_srep_pos_chc$city)
  roadsitems_srep_pos_chc$execution_date <- as.Date(roadsitems_srep_pos_chc$execution_date, format = "%d-%m-%Y")
  #roadsitems_srep_pos_chc$execution_date <<- as.Date(roadsitems_srep_pos_chc$execution_date, format = "%b %d %Y") #---
  return(roadsitems_srep_pos_chc)
}

#* Get data for the gauge chart graphic
#* @get /data_store_visited

function(salerep=NULL, idpos=NULL,idcluster=NULL,location=NULL,timestart=NULL,timeend=NULL){ #,posId,saleRep,channelCluster
  filtre_stv <-data_gauge()
  #filtre_stv$execution_date <- as.Date(filtre_stv$execution_date, format = "%d-%m-%Y")
  #filtre_stv$execution_date <- as.Date(filtre_stv$execution_date,format("%d-%m-%Y"))
  
  if (!is.null(salerep)){
    filtre_stv <- filtre_stv %>% filter(saleRep==salerep)
  }
  if (!is.null(idpos)){
    filtre_stv <- filtre_stv %>% filter(posId==idpos)
  }
  if (!is.null(idcluster)) {
    filtre_stv <- filtre_stv %>% filter(channelCluster==idcluster)
  }
  if (!is.null(location)) {
    filtre_stv <- filtre_stv %>% filter(city==location)
  }
  if (!is.null(timestart) & !is.null(timeend)) {
    filtre_stv <- filtre_stv %>% filter(execution_date>=timestart & execution_date<=timeend)
  }
  
  filtre_stv_comp <- filtre_stv %>% filter(status.x=="completed")
  nb_completed <- nrow(filtre_stv_comp)
  nb_all <- nrow(filtre_stv)
  values <-data.frame("nb_completed"=nb_completed,"nb_all"=nb_all)
  return (values)
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
  #filtre$execution_date <- as.Date(filtre$execution_date, format = "%b %d %Y")
  if (!is.null(salerep)){
    filtre <- filtre %>% filter(saleRep==salerep)
  }
  if (!is.null(idpos)){
    filtre <- filtre %>% filter(posId.x==idpos)
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
  filtre <- filtre %>% 
    filter(taskid_status=="completed" & time >=5) %>%
    group_by(name_salerep) %>%
    summarize(total_time=sum(time,na.rm = TRUE), total_performed=sum(time_performed,na.rm = TRUE))
  return (filtre)
}

#* Get data for the bar chart graphic of pos visited by channel cluster
#* @get /data_store_per_channelcluster

function(salerep=NULL, idpos=NULL,idcluster=NULL,location=NULL,timestart=NULL,timeend=NULL) { 
  filtre <- data_gauge()
  #filtre$execution_date <- as.Date(filtre$execution_date, format = "%b %d %Y")
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
  dataf <- filtre %>% filter(status.x=="completed") %>%
    group_by(name_cluster,channelCluster,color) %>%
    summarize(count=n())
  # dataf <- filtre %>% filter(status.x=="completed")
  # dataf <- as.data.frame(table(dataf$name))
  return (dataf)
}


#* @get /data_salerep_fixed_date
function(salerep=NULL, idpos=NULL,idcluster=NULL,location=NULL) { 
  filtre <-data_table()
  if (!is.null(salerep)){
    filtre <- filtre %>% filter(saleRep==salerep)
  }
  if (!is.null(idpos)){
    filtre <- filtre %>% filter(posId.x==idpos)
  }
  if (!is.null(idcluster)) {
    filtre <- filtre %>% filter(channelCluster==idcluster)
  }
  if (!is.null(location)) {
    filtre <- filtre %>% filter(city==location)
  }
  
  #today
  todayy <- filtre %>% filter(taskid_status=="completed" & time >=5) %>%
    filter(date_start<=Sys.Date() & date_start>=Sys.Date()) %>%
    group_by(name_salerep) %>% 
    summarise(total_time_today=sum(time,na.rm = TRUE),total_time_performed_today=sum(time_performed,na.rm = TRUE))
  
  #last 7 days
  last_7days <-filtre %>% filter(taskid_status=="completed" & time >=5) %>%
    filter(date_start<=Sys.Date() & date_start>=Sys.Date()-7) %>%
    group_by(name_salerep) %>% 
    summarise(total_time_7=sum(time,na.rm = TRUE),total_time_performed_7=sum(time_performed,na.rm = TRUE))
  
  #last 30 days
  last_30days <-filtre %>% filter(taskid_status=="completed" & time >=5) %>%
    filter(date_start<=Sys.Date() & date_start>=Sys.Date()-30) %>%
    group_by(name_salerep) %>% 
    summarise(total_time_30=sum(time,na.rm = TRUE),total_time_performed_30=sum(time_performed,na.rm = TRUE))
  
  #this year  
  this_year <- filtre %>% filter(taskid_status=="completed" & time >=5) %>%
    filter(date_start<=Sys.Date() & date_start>="2024-01-01") %>%
    group_by(name_salerep) %>% 
    summarise(total_time_year=sum(time,na.rm = TRUE),total_time_performed_year=sum(time_performed,na.rm = TRUE))
  
  #last year 2023
  last_year <- filtre %>% filter(taskid_status=="completed" & time >=5) %>%
    filter(date_start<="2023-12-01" & date_start>="2023-01-01") %>%
    group_by(name_salerep) %>% 
    summarise(total_time_lastyear=sum(time,na.rm = TRUE),total_time_performed_lastyear=sum(time_performed,na.rm = TRUE))
  
  data_aggregate <- Reduce(function(x, y) merge(x, y, by = "name_salerep", all = TRUE), list(todayy, last_7days, last_30days,last_year,this_year))
  return(data_aggregate)
}