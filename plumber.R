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

flatten_taskid <- function(df) {
  df %>%
    mutate(taskid = map(taskIds, as_tibble)) %>%
    unnest(taskid, names_sep = "_")
}

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

data <- function(){
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
  # return(list("activitieitems"=activitieitems, "activities"=activities, "categories"=categories, "channelclusters"=channelclusters,
  #             "companies"=companies, "pos"=pos, "road_items"=road_items,
  #        "roads"=roads, "salereps"=salereps))
}

# for (table_name in list_tables) {
#   assign(table_name, dataframes[[table_name]]) 
# }

#* Get data for the gauge chart graphic
#* @get /data_store_visited

function(salerep=NULL, namepos=NULL,cluster=NULL,location=NULL,timestart=NULL,timeend=NULL){ #,daterange,location,channelcluster
  # for (table_name in list_tables) {
  #   assign(table_name, fetch_data()[[table_name]])
  # }
  # roadsitems <- merge(road_items, roads,by.x = "roadId",by.y="_id",all.x = TRUE)
  # roadsitems <- roadsitems %>% select(roadId,`_id`,posId,
  #                                     id_company.x, saleRep, roadItems,status.x,execution_date) %>%
  #   rename(id_road_items=`_id`,id_company=id_company.x,status=status.x)
  # 
  # #join the precedent dataframe with salereps
  # roadsitems_srep <- merge(roadsitems,salereps,by.x = "saleRep", by.y = "_id",all.x = TRUE) 
  # roadsitems_srep <- roadsitems_srep %>% select(roadId, id_road_items,roadItems,saleRep,id_company.x,status.x,execution_date,name,posId)
  # 
  # #join the precedent dataframe with pos
  # roadsitems_srep_pos <- merge(roadsitems_srep,pos,by.x="posId", by.y="_id",all.x = TRUE)  
  # roadsitems_srep_pos <- roadsitems_srep_pos %>% select(roadId,id_road_items,roadItems,saleRep,id_company.x,status.x,execution_date,name.x,name.y,posId,
  #                                                       city,channelCluster) %>%
  #   rename(name_salerep=name.x,name_pos=name.y)
  # 
  # #join the precedent dataframe with channelclusters
  # roadsitems_srep_pos_chc <- merge(roadsitems_srep_pos,channelclusters,by.x="channelCluster",by.y = "_id",all.x = TRUE)
  # roadsitems_srep_pos_chc <- roadsitems_srep_pos_chc %>% select(roadId,id_road_items,roadItems,saleRep,id_company,status.x,execution_date,name_salerep,posId,name_pos,
  #                                                               city,channelCluster,name) %>% 
  #   rename(name_cluster=name)
  # 
  # #rename the modalities of variable city
  # roadsitems_srep_pos_chc$city <- gsub("Yaoundé, Cameroon","Yaounde",roadsitems_srep_pos_chc$city)
  roadsitems_srep_pos_chc <<- data()
  filtre <-roadsitems_srep_pos_chc
  if (!is.null(salerep)){
     filtre <- filtre %>% filter(name_salerep==salerep)
  }
  if (!is.null(namepos)){
     filtre <- filtre %>% filter(name_pos==namepos)
  }
  if (!is.null(cluster)) {
    filtre <- filtre %>% filter(name_cluster==cluster)
  }
  if (!is.null(location)) {
    filtre <- filtre %>% filter(city==location)
  }
  if (!is.null(timestart) & !is.null(timeend)) {
    filtre <- filtre %>% filter(execution_date>=timestart & execution_date<=timeend)
  }
  return (filtre)
}

#* @get /data_sales_rep
function() {
  df4 <- roadsitems_srep_pos_chc
  df5 <- merge(df4,road_items,by.x = "id_road_items",by.y = "_id")
  df5$id_company.x <- NULL
  final_df <- flatten_taskid(df5)
  
  final_df <- final_df %>%
    mutate(starttime=str_extract(taskid_startTime, "\\d{2}:\\d{2}:\\d{2}")) %>%
    mutate(endtime=str_extract(taskid_endTime, "\\d{2}:\\d{2}:\\d{2}")) 
  final_df$starttime <- times(final_df$starttime)
  final_df$endtime <- times(final_df$endtime)
  final_df$gap <- final_df$endtime -final_df$starttime
   
  
  final_df <- final_df %>%
    mutate(date_task=str_extract(taskid_startTime, "\\w{3} \\d{2} \\d{4}"))
  final_df$date_task <- as.Date(final_df$date_task, format = "%b %d %Y")
  
  #taskactivityitem et idactivityitem
  final_df1 <<- merge(final_df,activitieitems,by.x = "taskid_activityItem", by.y="_id")
  final_df1$status.x <- NULL
  minutes <- hms(final_df1$gap)        # format to 'hours:minutes:seconds'
  final_df1$gap <- hour(minutes)*60 + minute(minutes) 
  final_df1$starttime <- as.character(final_df1$starttime)
  final_df1$endtime <- as.character(final_df1$endtime)
  return(final_df1)
}

#* @get /data2
function( ) {
  return(activitieitems)
}