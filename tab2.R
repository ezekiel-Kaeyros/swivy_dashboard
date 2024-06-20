Sys.setlocale("LC_ALL","English")
library(purrr)
library(chron)

# df5 <- df4 %>%
#   unnest(roadItems)

df5 <- merge(df4,road_items,by.x = "id_road_items",by.y = "_id")


flatten_taskid <- function(df) {
  df %>%
    mutate(taskid = map(taskIds, as_tibble)) %>%
    unnest(taskid, names_sep = "_")
}

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
final_df1 <- merge(final_df,activitieitems,by.x = "taskid_activityItem", by.y="_id")
