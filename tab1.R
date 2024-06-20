library(tidyr)
library(dplyr)

# roads <- roads %>%
#   unnest(roadItems)

#df1 <- inner_join(road_items,roads,by=c("roadId"="_id"))

df1 <- merge(road_items, roads,by.x = "roadId",by.y="_id",all.x = TRUE)
df1 <- df1 %>% select(roadId,`_id`,posId,
                      id_company.x, saleRep, roadItems,status.x,execution_date) %>%
  rename(id_road_items=`_id`,id_company=id_company.x,status=status.x)


df2 <- merge(df1,salereps,by.x = "saleRep", by.y = "_id",all.x = TRUE) 
df2 <- df2 %>% select(roadId, id_road_items,roadItems,saleRep,id_company.x,status.x,execution_date,name,posId)
                      #taskIds,posId,arrivalTimeAtPOS,leavingTimeFromPOS)

df3 <- merge(df2,pos,by.x="posId", by.y="_id",all.x = TRUE)  
df3 <- df3 %>% select(roadId,id_road_items,roadItems,saleRep,id_company.x,status.x,execution_date,name.x,name.y,posId,
               city,channelCluster) %>%
  rename(name_salerep=name.x,name_pos=name.y)

df4 <- merge(df3,channelclusters,by.x="channelCluster",by.y = "_id",all.x = TRUE)
df4 <- df4 %>% select(roadId,id_road_items,roadItems,saleRep,id_company,status.x,execution_date,name_salerep,posId,name_pos,
                      city,channelCluster,name) %>% 
  rename(name_cluster=name)
df4$city <- gsub("Yaoundé, Cameroon","Yaounde",df4$city)


# agent <- df4 %>%
#   filter(name_salerep=="Biteb Stephane") %>% #Modern Trade Retail
#   filter(name_cluster=="Modern Trade Retail") 
# 
# number_road <- length(unique(agent$id_road_items))
