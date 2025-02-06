rm(list = ls())
library(dplyr)
library(lubridate)

## this script identifies overlapping time windows of drivers for each flood segment within an individual flood events
## for example, if a flood event has multiple peaks, it will be separated into multiple flood windows (based on the 95p)
## threshold, and each flood window will be compared with flooding windows from other drivers/river location to identify
## the overlapping period of time. In comparison, a simple approach is to take the multi-peak period as a single flood window.

# read daily flow data  ---------------------------------------------------------
scenario <- "hist"
file_path <- sprintf("C:\\Users\\sunn067\\OneDrive - PNNL\\iCoM\\manuscripts\\paper 4 Long-term Flooding\\dataset\\%s.csv", scenario)
df <- read.csv(file_path, stringsAsFactors = FALSE, header = FALSE)
# Convert the first column to a Date type
df[, 1] <- as.Date(df[, 1], format = "%d/%m/%Y")
# Ensure the other columns are treated as floats
for (i in 2:ncol(df)) {
  df[, i] <- as.numeric(df[, i])
}
flow <- df[, c("V1", "V2", "V3")]
flow$V1 <- as.Date(flow$V1)

# read metrics calculated for each flood event (extracted by other scripts - see my tutorial note) -------------------------------------
dir <- "C:/Users/sunn067/OneDrive - PNNL/iCoM/manuscripts/paper 4 Long-term Flooding/flood_events/all_events/"

V2 <- read.csv(paste0(dir,"V2_Schulykill_all_metrics.csv"))
V3 <- read.csv(paste0(dir,"V3_Trenton_all_metrics.csv"))
V3$Peak_date <- as.Date(V3$Peak_date, format="%m/%d/%Y")
V3$Begin <- as.Date(V3$Begin, format="%m/%d/%Y")
V3$End <- as.Date(V3$End, format="%m/%d/%Y")

# Filter flow data based on threshold
percentiles <- c(0.95)
v2_threshold <- quantile(flow$V2, percentiles)
v3_threshold <- quantile(flow$V3, percentiles)


# Loop through each event in V2Flood ---------------
results <- data.frame(OrigID=integer(), Segment_Begin=as.Date(character()), Segment_End=as.Date(character()), Duration=integer(), stringsAsFactors=FALSE)
for (i in 1:nrow(V2)) {
  event <- V2[i,]
  # Filter flow data within the event's duration
  event_flow <- flow[flow$V1 >= event$Begin & flow$V1 <= event$End,]
  
  if (nrow(event_flow) > 0) {
    # Find the dates where flow exceeds the threshold
    filtered_dates = event_flow$V1[event_flow$V2 > v2_threshold]
    filtered_flow = event_flow$V2[event_flow$V2 > v2_threshold]

    # Initialize segment start and end with the first exceeding element
    segment_start = filtered_dates[1]
    segment_end = tail(filtered_dates, 1)
   
    # End the current segment and save it
    duration = as.numeric(segment_end - segment_start) + 1
    new_row <- data.frame(OrigID=event$OrigID, Segment_Begin=as.Date(segment_start), Segment_End=as.Date(segment_end), Duration=duration, stringsAsFactors=FALSE)
    results <- rbind(results, new_row) 
  }
}
V2Flood <- unique(results)
#dir <- "C:/Users/sunn067/OneDrive - PNNL/iCoM/manuscripts/paper 4 Long-term Flooding/flood_events/all_events/"
#write.csv(V2Flood, paste0(dir,"V2_Schulykill_flooding_window.csv"), row.names = FALSE)

# Loop through each event in V3 -----------------------------
# Initialize an empty data frame to store the results with appropriate data types
results <- data.frame(OrigID=integer(), Segment_Begin=as.Date(character()), Segment_End=as.Date(character()), Duration=integer(), stringsAsFactors=FALSE)

for (i in 1:nrow(V3)) {
  event <- V3[i,]
  # Filter flow data within the event's duration
  event_flow <- flow[flow$V1 >= event$Begin & flow$V1 <= event$End,]
  
  if (nrow(event_flow) > 0) {
    # Find the dates where flow exceeds the threshold
    filtered_dates = event_flow$V1[event_flow$V3 > v3_threshold]
    filtered_flow = event_flow$V3[event_flow$V3 > v3_threshold]
    
    segment_start = filtered_dates[1]
    segment_end = tail(filtered_dates, 1)
      
    duration = as.numeric(segment_end - segment_start) + 1
    new_row <- data.frame(OrigID=event$OrigID, Segment_Begin=as.Date(segment_start), Segment_End=as.Date(segment_end), Duration=duration, stringsAsFactors=FALSE)
    results <- rbind(results, new_row)  
  }
}
V3Flood <- unique(results)
dir <- "C:/Users/sunn067/OneDrive - PNNL/iCoM/manuscripts/paper 4 Long-term Flooding/flood_events/all_events/"
#write.csv(V3Flood, paste0(dir,"V3_Trenton_flooding_window.csv"), row.names = FALSE)


# merge two lists  ------------
combined_floods <- list()
V2Flood <- merge(V2Flood, V2, by="OrigID")
V3Flood <- merge(V3Flood, V3, by="OrigID")

V2Flood$Begin <- as.Date(V2Flood$Begin)
V2Flood$End <- as.Date(V2Flood$End)
V3Flood$Begin <- as.Date(V3Flood$Begin)
V3Flood$End <- as.Date(V3Flood$End)


# initializing a boolean vector to track which V3Flood events are overlapped
v3_overlapped <- rep(FALSE, nrow(V3Flood))
for(i in 1:nrow(V2Flood)) {
  overlap <- which(V3Flood$Segment_Begin <= V2Flood$Segment_End[i] & V3Flood$Segment_End >= V2Flood$Segment_Begin[i])
  if(length(overlap) > 0) {
    overlap_duration <- as.numeric(difftime(min(V2Flood$Segment_End[i], V3Flood$Segment_End[overlap]),
                                            max(V2Flood$Segment_Begin[i], V3Flood$Segment_Begin[overlap]), units = "days")) + 1
    actual_begin  = min(V2Flood$Begin[i], V3Flood$Begin[overlap])
    actual_end  = max(V2Flood$End[i], V3Flood$End[overlap])
    # If overlapping events exist, merge them and mark the V3 event as overlapped
    combined_event <- data.frame(
      OrigID_V2 = V2Flood$OrigID[i],
      OrigID_V3 = V3Flood$OrigID[overlap],
      EventBegin = actual_begin,
      EventEnd = actual_end,
      
      V2DailyMQ = V2Flood$DailyMQ[i],
      V2PeakDate = V2Flood$Peak_date[i],
      V2FloodBegin = V2Flood$Segment_Begin[i],
      V2FloodEnd = V2Flood$Segment_End[i],
      V2_KGE = V2Flood$KGE[i],
      V2_Pbias = V2Flood$PeakBias[i],
      V3DailyMQ = V3Flood$DailyMQ[overlap],
      V3PeakDate = V3Flood$Peak_date[overlap],
      V3FloodBegin = V3Flood$Segment_Begin[overlap],
      V3FloodEnd = V3Flood$Segment_End[overlap],
      V3_KGE = V3Flood$KGE[overlap],
      V3_Pbias = V3Flood$PeakBias[overlap],
      Type = "combined",
      Duration = as.numeric(difftime(actual_end, actual_begin, units = "days")) + 1,
      Overlap_Days = overlap_duration,
      Overlap_Start = max(V2Flood$Segment_Begin[i], V3Flood$Segment_Begin[overlap]),
      Overlap_End = min(V2Flood$Segment_End[i], V3Flood$Segment_End[overlap])
    )
    combined_floods <- append(combined_floods, list(combined_event))
    
    v3_overlapped[overlap] <- TRUE
  } else {
    # No overlap exists for V2
    single_event <- data.frame(
      OrigID_V2 = V2Flood$OrigID[i],
      OrigID_V3 = NA,
      EventBegin = V2Flood$Begin[i],
      EventEnd = V2Flood$End[i],
      V2DailyMQ = V2Flood$DailyMQ[i],
      V2PeakDate = V2Flood$Peak_date[i],
      V2FloodBegin = V2Flood$Segment_Begin[i],
      V2FloodEnd = V2Flood$Segment_End[i],
      V2_KGE = V2Flood$KGE[i],
      V2_Pbias = V2Flood$PeakBias[i],
      V3DailyMQ = NA,
      V3PeakDate = NA,
      V3FloodBegin = NA,
      V3FloodEnd = NA,
      V3_KGE = NA,
      V3_Pbias = NA,
      Type = "V2",
      Duration = V2Flood$Duration.y[i],  # Assuming Duration is correctly set in V2Flood
      Overlap_Days = 0,  # Assuming no overlap
      Overlap_Start = NA,
      Overlap_End = NA
    )
    combined_floods <- append(combined_floods, list(single_event))
  }
}
# Add non-overlapping V3Flood events to the combined_floods list
for(j in 1:nrow(V3Flood)) {
  if(!v3_overlapped[j]) {
    single_event_v3 <- data.frame(
      OrigID_V2 = NA,
      OrigID_V3 = V3Flood$OrigID[j],
      EventBegin = V3Flood$Begin[j],
      EventEnd = V3Flood$End[j],
      V2DailyMQ = NA,
      V2PeakDate = NA,
      V2FloodBegin = NA,
      V2FloodEnd = NA,
      V2_KGE = NA,
      V2_Pbias = NA,
      V3DailyMQ = V3Flood$DailyMQ[j],
      V3PeakDate = V3Flood$Peak_date[j],
      V3FloodBegin = V3Flood$Segment_Begin[j],
      V3FloodEnd = V3Flood$Segment_End[j],
      V3_KGE = V3Flood$KGE[j],
      V3_Pbias = V3Flood$PeakBias[j],
      Type = "V3",
      Duration = V3Flood$Duration.y[j],
      Overlap_Days = 0, 
      Overlap_Start = NA,
      Overlap_End = NA
    )
    combined_floods <- append(combined_floods, list(single_event_v3))
  }
}
# Combine the resulting list of data frames - currently having 262 events
combined_floods_df <- do.call(rbind, combined_floods)



# remove duplicates and merge rows with the same event ID ----------

# Convert dates to Date objects if they are not already -----
combined_floods_df$EventBegin <- as.Date(combined_floods_df$EventBegin, format="%m/%d/%Y")
combined_floods_df$EventEnd <- as.Date(combined_floods_df$EventEnd, format="%m/%d/%Y")
combined_floods_df$V2FloodBegin <- as.Date(combined_floods_df$V2FloodBegin, format="%m/%d/%Y")
combined_floods_df$V2FloodEnd <- as.Date(combined_floods_df$V2FloodEnd, format="%m/%d/%Y")
combined_floods_df$V3FloodBegin <- as.Date(combined_floods_df$V3FloodBegin, format="%m/%d/%Y")
combined_floods_df$V3FloodEnd <- as.Date(combined_floods_df$V3FloodEnd, format="%m/%d/%Y")
combined_floods_df$Overlap_Start <- as.Date(combined_floods_df$Overlap_Start, format="%m/%d/%Y")
combined_floods_df$Overlap_End <- as.Date(combined_floods_df$Overlap_End, format="%m/%d/%Y")

na_origid_v3_df <- combined_floods_df %>% filter(is.na(OrigID_V3))
non_na_origid_v3_df <- combined_floods_df %>% filter(!is.na(OrigID_V3))
summarized_df <- non_na_origid_v3_df %>% 
  group_by(OrigID_V3) %>%
  summarise(
    OrigID_V2 = first(na.omit(OrigID_V2)),
    EventBegin = min(EventBegin),
    EventEnd = max(EventEnd),
    V2DailyMQ = max(V2DailyMQ),  # assuming keeping the first one is ok
    V2PeakDate = first(V2PeakDate[which.max(V2DailyMQ)]),
    V2FloodBegin = min(V2FloodBegin),
    V2FloodEnd = max(V2FloodEnd),
    V2_KGE = max(V2_KGE),
    V2_Pbias = min(V2_Pbias),
    V3DailyMQ = first(V3DailyMQ),
    V3PeakDate = first(V3PeakDate),
    V3FloodBegin = first(V3FloodBegin),
    V3FloodEnd = first(V3FloodEnd),
    V3_KGE = first(V3_KGE),
    V3_Pbias = first(V3_Pbias),
    Type = if("combined" %in% Type) "combined" else first(Type),
    Duration = as.numeric(max(EventEnd) - min(EventBegin))+1,
    Overlap_Days = as.numeric(max(Overlap_End)-min(Overlap_Start))+1,
    Overlap_Start = min(Overlap_Start),
    Overlap_End = max(Overlap_End),
    .groups = 'drop'
  )
combined_floods_df4 <- bind_rows(summarized_df, na_origid_v3_df)


#combined_floods_df$OrigID <- NULL
combined_floods_df4 <- combined_floods_df4 %>%
  mutate(RowNumber = row_number())
combined_floods_df4 <- combined_floods_df4 %>%
  rename(ID = RowNumber) %>%
  select(ID, everything())
write.csv(combined_floods_df4, paste0(dir,"river_floods_grouping_v2.csv"), row.names = FALSE, quote=FALSE)

# ---------------

# Combine the resulting list of data frames - currently having 262 events
type_summary <- combined_floods_df4 %>%
  group_by(Type) %>%
  summarise(RecordCount = n())
print(type_summary)


# Find all duplicate OrigID_V2 values (including the first occurrence)
duplicate_IDs <- combined_floods_df$OrigID_V3[duplicated(combined_floods_df$OrigID_V3) | duplicated(combined_floods_df$OrigID_V3, fromLast = TRUE)]
unique_duplicate_IDs <- unique(duplicate_IDs)
print(unique_duplicate_IDs)