## This code generates temperature trends for the Metaxas Lab study sites.
## Set your working directory and ensure all required .csv files are present.
## Install required libraries using install.packages() function.

#### Bringing in Data ####
SC_data <- read.csv("SC_Temps_All.csv")
TL_data <- read.csv("TL_Temps_All.csv")
MO_data <- read.csv("MO_Temps_All.csv")
LI_data <- read.csv("LI_Temps_All.csv")
HB_data <- read.csv("HB_Temps_All.csv")

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

# Combine the datasets
all_data <- bind_rows(SC_data, TL_data, MO_data, LI_data, HB_data)

# Group data by Site, Depth, and Date, then calculate the mean Temperature
all_data <- all_data %>%
  group_by(Site, Depth, Date) %>%
  summarize(Temperature = mean(Temperature, na.rm = TRUE)) %>%
  ungroup()

# Create new Site_Depth Column
all_data <- all_data %>% 
  mutate(Site_Depth = paste(Site, Depth, sep = "_"))

# Reorder columns
all_data <- all_data %>%
  select(Site, Depth, Site_Depth, Date, Temperature)

# Extract year and date information
all_data <- all_data %>%
  mutate(Year = year(ymd(Date)),
         Date = as.Date(Date))

# Identify gaps in the data
identify_gaps <- function(data) {
  data <- data %>% arrange(Date)
  data <- data %>%
    mutate(Next_Date = lead(Date),
           Gap = ifelse(difftime(Next_Date, Date, units = "days") > 1, TRUE, FALSE))
  gaps <- data %>% 
    filter(Gap == TRUE) %>%
    select(Date, Next_Date) %>%
    rename(Start = Date, End = Next_Date)
  return(gaps)
}

# Group by Site_Depth and identify gaps
years_with_gaps <- all_data %>%
  group_by(Site_Depth) %>%
  do(identify_gaps(.)) %>%
  ungroup()


# Function to remove years with gaps and extreme years
remove_years_with_gaps_and_extremes <- function(data, site_depth) {
  site_data <- data %>% filter(Site_Depth == site_depth)
  gap_info <- find_data_gaps(site_data)
  
  filtered_data <- data %>%
    filter(Site_Depth == site_depth) %>%
    group_by(Year) %>%
    summarize(
      Max_Temp = max(Temperature, na.rm = TRUE),
      Min_Temp = min(Temperature, na.rm = TRUE),
      Max_Date = Date[which.max(Temperature)],
      Min_Date = Date[which.min(Temperature)],
      Site_Depth = first(Site_Depth)
    ) %>%
    filter(!Year %in% gap_info$years_with_gaps)
  
  # Remove oldest and most recent years
  filtered_data %>%
    arrange(Year) %>%
    slice(2:(n()-1))
}

# Get linear model info
get_lm_info <- function(data, x, y) {
  model <- lm(data[[y]] ~ data[[x]])
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  r_squared <- summary(model)$r.squared
  
  list(
    equation = sprintf("y = %.4fx + %.2f", slope, intercept),
    r_squared = sprintf("R² = %.3f", r_squared)
  )
}

# Function to plot temperature trends
plot_temp_trends <- function(data, site_depth) {
  site_data <- data %>% filter(Site_Depth == site_depth)
  gap_info <- find_data_gaps(site_data)
  max_min_data <- remove_years_with_gaps_and_extremes(data, site_depth)
  
  # Filter gaps for the specific site_depth
  site_gaps <- years_with_gaps %>%
    filter(Site_Depth == site_depth)
  
  max_lm_info <- get_lm_info(max_min_data, "Max_Date", "Max_Temp")
  min_lm_info <- get_lm_info(max_min_data, "Min_Date", "Min_Temp")
  
  y_range <- range(site_data$Temperature, na.rm = TRUE)
  y_padding <- diff(y_range) * 0.1
  y_min <- floor(y_range[1] - y_padding)
  y_max <- ceiling(y_range[2] + y_padding)
  
  p <- ggplot(site_data, aes(x = Date, y = Temperature)) +
    geom_line(color = "black", alpha = 0.5)
  
  if(nrow(gap_info$gaps) > 0) {
    p <- p +  geom_rect(data = site_gaps, aes(xmin = as.Date(Start), xmax = as.Date(End), ymin = -Inf, ymax = Inf),
                        fill = "red", alpha = 0.3, inherit.aes = FALSE)
  }
  
  p +
    geom_point(data = max_min_data, aes(x = Max_Date, y = Max_Temp), color = "red", size = 1) +
    geom_point(data = max_min_data, aes(x = Min_Date, y = Min_Temp), color = "blue", size = 1) +
    geom_smooth(data = max_min_data, aes(x = Max_Date, y = Max_Temp), method = "lm", color = "red", se = FALSE, size = 0.5, linetype = "longdash") +
    geom_smooth(data = max_min_data, aes(x = Min_Date, y = Min_Temp), method = "lm", color = "blue", se = FALSE, size = 0.5, linetype = "longdash") +
    annotate("text", x = min(site_data$Date), y = y_max, 
             label = paste(max_lm_info$equation, max_lm_info$r_squared, sep = "\n"), 
             hjust = 0, vjust = 1, color = "red", size = 2) +
    annotate("text", x = min(site_data$Date), y = y_min, 
             label = paste(min_lm_info$equation, min_lm_info$r_squared, sep = "\n"), 
             hjust = 0, vjust = 0, color = "blue", size = 2) +
    labs(title = paste("Temperature Trends for", site_depth),
         x = "Date",
         y = "Temperature (°C)") +
    theme_classic() +
    coord_cartesian(ylim = c(y_min, y_max)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(-2, 20, by = 2))
}

# Generate plots
site_depths <- unique(all_data$Site_Depth)

pdf("South Shore and ESI Logger Data.pdf", width = 12, height = 8)

for (i in seq(1, length(site_depths), 4)) {
  plots <- map(site_depths[i:min(i+3, length(site_depths))], ~plot_temp_trends(all_data, .x))
  print(wrap_plots(plots, ncol = 2))
}

dev.off()