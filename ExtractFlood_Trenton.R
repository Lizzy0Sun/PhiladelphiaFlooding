rm(list = ls())

library("FloodR")
library(dplyr)
library(RColorBrewer)
library(viridis)
library(lubridate)
library(ggplot2)
library(plotly)
library(patchwork)
library(purrr)
library("extRemes")


# read data  ---------------------------------------------------------
scenario <- "hist"
file_path <- sprintf("C:\\Users\\sunn067\\OneDrive - PNNL\\iCoM\\manuscripts\\paper 4 Long-term Flooding\\dataset\\%s.csv", scenario)
df <- read.csv(file_path, stringsAsFactors = FALSE, header = FALSE)

# Convert the first column to a Date type
df[, 1] <- as.Date(df[, 1], format = "%d/%m/%Y")

# Ensure the other columns are treated as floats
for (i in 2:ncol(df)) {
  df[, i] <- as.numeric(df[, i])
}

# get the annual maximum for each data column
annual_max <- df %>%
  group_by(Year = year(V1)) %>%
  summarise(across(starts_with("V"), max, na.rm = TRUE))
print(annual_max)

# get the percentiles for each data column
percentiles <- c(0.75, 0.90, 0.95, 0.99, 1)
v2_percentiles <- quantile(df$V2, percentiles)
v3_percentiles <- quantile(df$V3, percentiles)


# Fit the GEV distribution to get the 2-year event (used as the flood threshold)
columns <- paste0("V", 2:9)
return_levels <- list()
for (col in columns) {
  fit <- fevd(annual_max[[col]], type = "GEV")
  # Estimate the 2-year return level
  rl <- return.level(fit, return.period = c(2, 10, 20, 50, 100))
  return_levels[[col]] <- rl
}

# get the minimum of annual maximum for each data column
ann_min <- annual_max %>%
  summarize(across(V2:V9, min))

ann_max <- annual_max %>%
  summarize(across(V2:V9, max))


# identify floods for each main reach  ----------------------------------------
# Loop through columns from V2 to V9
selected_columns <- c("V3")
legend <- c(V3 = "Trenton_4115")
Flood_events <- list()
dailyQ <- list()
for (col_name in selected_columns) { 
  
  dailyQ[[col_name]] <- df %>% select(V1, col_name)
  events <- eventsep(dailyQ[[col_name]])
  
  # peak < the given threshold is removed from the flood events
  threshold <- ann_min[[col_name]][1]
  #threshold <- unname(return_levels[[col_name]][1])
  print(threshold)
  tmp <- events %>%
    filter(DailyMQ >= threshold)
  
  # add the duration of each flood event
  Flood_events[[col_name]] <- tmp %>%
    mutate(Duration = as.numeric(End - Begin) + 1)
}

# group the data based on criteria ----------------------------
# G1: 1 peak and duration < 10 days
df <- as.data.frame(Flood_events[[col_name]])
# Add the Original_ID columns
df <- df %>%
  mutate(Original_ID = row_number())
# Add the Duration column
df$Duration <- as.numeric(df$End - df$Begin + 1)
# Filter the data frame
G1 <- df %>%
  filter(No_Peaks == 1 & Duration < 10)

# G2: 1 peak and duration > 10 days
G2 <- df %>%
  filter(No_Peaks == 1 & Duration > 10)

# G3: 2 peaks
G3 <- df %>%
  filter(No_Peaks == 2)

# G4: >2 peaks
G4 <- df %>%
  filter(No_Peaks > 2)

# Plot---------------------------------------------------------
# Schuylkill_4622 first -> V2
col_name <- "V3"
dQ<- rename(dailyQ[[col_name]], Date = V1, Discharge = all_of(col_name))
color_palette <- viridis(1)
plot_list <- list()
# Extract and plot data for each flood event
for (i in 1:nrow(Flood_events[[col_name]])) {
  
  start_date <- Flood_events[[col_name]]$Begin[i]
  end_date <- Flood_events[[col_name]]$End[i]
  
  subset_data <- dQ %>%
    filter(Date >= start_date & Date <= end_date)
  
  p <- ggplot(subset_data, aes(x = Date, y = Discharge, color = "Trenton")) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Trenton" = color_palette[1])) +
    labs(title = paste("Trenton Flood Event", i, ":", start_date, "to", end_date),
         x = "Date",
         y = "Discharge") +
    theme_minimal()+
    theme(legend.position = "none")
  
  plot_list[[i]] <- p
  
}

# Print a set of events based on the indices in G2$Original_ID ------------
#selected_indices <- c(1, 3, 9, 10, 12, 16)
#selected_plots <- plot_list[selected_indices]
selected_plots <- plot_list[G4$Original_ID]
combined_plot <- wrap_plots(selected_plots, ncol = 5)  # 2 columns or change as per your preference
ggsave("C:/Users/sunn067/Desktop/G4_PLOT.png", combined_plot, width = 24, height = 30)


# export plots for a single event --------------------------------------------------------------
print(plot_list[[39]])
file_name <- paste0("C:\\Users\\sunn067\\Desktop\\flood",i,".png")
ggsave(file_name, plot = p, width = 6, height = 4, dpi = 300)

# save the flood events to csv
write.csv(Flood_events["V3"], "C:/Users/sunn067/Desktop/Flood_events_V3.csv", row.names = FALSE)

