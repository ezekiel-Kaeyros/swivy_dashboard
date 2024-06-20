library(mongolite)

mongo_url <- "mongodb+srv://eric2mballus:tititata85@cluster0.e1pkdu7.mongodb.net/swivy_db"

list_tables <- c("activitieitems", "activities", "categories", "channelclusters", "companies", "pos", "road_items",
                 "roads", "salereps")

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

activitieitems <- dataframes$activitieitems
activities <- dataframes$activities
categories <- dataframes$categories
channelclusters <- dataframes$channelclusters
companies <- dataframes$companies
pos <- dataframes$pos
road_items <- dataframes$road_items
roads <- dataframes$roads
salereps <- dataframes$salereps
# 666a7e71c0a6a3c9f20aff8a