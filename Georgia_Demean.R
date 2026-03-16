library(purrr)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(spdep)
library(sf)
library(dplyr)
library(sp)
library(plm)
library(reshape2)
library(sf)
library(egg)
library(grid)
library(rstudioapi)
library(cowplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############### Data Preprocessing ################
# Process each year of data from the 2016–2020 American Community Survey (ACS) 5-year estimates in a loop
# The processed file is already stored in the Input folder. Skip the code below if you want to use the processed data directly.

years <- 2016:2020

for (year in years) {
  # Read county data
  county <- read.csv("county.csv", stringsAsFactors = F)
  county <- county[county$STATEFP == 13,]
  county <- county[,c('GEOID_Data', 'GEOID','x','y','mile2')]
  colnames(county) <- c('id', 'GEOID','x','y','mile2')
  
  # Read yearly datasets
  x1 <- read.csv(paste0(year, "x1.csv"), stringsAsFactors = F)
  x2 <- read.csv(paste0(year, "x2.csv"), stringsAsFactors = F)
  x5 <- read.csv(paste0(year, "x5.csv"), stringsAsFactors = F)
  x6 <- read.csv(paste0(year, "x6.csv"), stringsAsFactors = F)
  x7 <- read.csv(paste0(year, "x7.csv"), stringsAsFactors = F)
  x23 <- read.csv(paste0(year, "x23.csv"), stringsAsFactors = F)
  x25 <- read.csv(paste0(year, "x25.csv"), stringsAsFactors = F)
  # Merge datasets
  county <- merge(county, x1, by.x = 'id', by.y = 'GEOID', all.x = T)
  county <- merge(county, x2, by.x = 'id', by.y = 'GEOID', all.x = T)
  county <- merge(county, x5, by.x = 'id', by.y = 'GEOID', all.x = T)
  county <- merge(county, x6, by.x = 'id', by.y = 'GEOID', all.x = T)
  county <- merge(county, x7, by.x = 'id', by.y = 'GEOID', all.x = T)
  county <- merge(county, x23, by.x = 'id', by.y = 'GEOID', all.x = T)
  county <- merge(county, x25, by.x = 'id', by.y = 'GEOID', all.x = T)
  
  # Compute variables
  county[[paste0("popu", year)]] <- county$B01001e1
  county[[paste0("age65", year)]] <- (county$B01001e20 + county$B01001e21 + county$B01001e22 + 
                                        county$B01001e23 + county$B01001e24 + county$B01001e25 + 
                                        county$B01001e44 + county$B01001e45 + county$B01001e46 + 
                                        county$B01001e47 + county$B01001e48 + county$B01001e49) / 
    county[[paste0("popu", year)]]
  
  county[[paste0("black", year)]] <- county$B02001e3 / county[[paste0("popu", year)]]
  county[[paste0("bachelor", year)]] <- county$B23006e23 / county[[paste0("popu", year)]]
  county[[paste0("lnpop", year)]] <- log(county[[paste0("popu", year)]] / county$mile2)
  county[[paste0("fb", year)]] <- county$B05002e13 / county[[paste0("popu", year)]]
  county[[paste0("rural", year)]] <- 1 - county$B07201e1 / county[[paste0("popu", year)]]
  county[[paste0("poverty", year)]] <- county$B06012e2 / county$B06012e1 
  county[[paste0("income", year)]] <- county$B25099e1
  
  # Select relevant columns
  data_year <- county[, c('GEOID','x','y', paste0("bachelor", year), paste0("popu", year), paste0("lnpop", year), 
                          paste0("age65", year), paste0("black", year), paste0("fb", year), 
                          paste0("rural", year), paste0("poverty", year), paste0("income", year))]
  
  colnames(data_year) <-  c('GEOID','x','y', 'bachelor', 'popu', 'lnpop', 'age65', 'black', 'fb', 'rural', 'poverty','income')
  # Add year column
  data_year$year <- year
  
  
  # Assign the dataframe a dynamic name (e.g., data_2016, data_2017, etc.)
  assign(paste0("data_", year), data_year)
  
  # Save the output file
  write.csv(data_year, paste0("Georgia", year, ".csv"), row.names = F)
}


data <- rbind(data_2016,data_2017,data_2018,data_2019, data_2020)

ids_to_remove <- unique(data$GEOID[rowSums(is.na(data)) > 0])
data <- data %>% filter(!GEOID %in% ids_to_remove)

for (i in c('bachelor', 'popu', 'lnpop', 'age65', 'black', 'fb', 'rural', 'poverty', 'income')) {
  data[[i]] <- as.numeric(scale(data[[i]]))  # Ensuring it remains a numeric vector
}


data <- data %>%
  group_by(GEOID) %>%
  mutate(bachelor_mean = mean(bachelor),
         fb_mean = mean(fb),
         age65_mean = mean(age65),
         black_mean = mean(black),
         rural_mean = mean(rural),
         income_mean = mean(income),
         poverty_mean = mean(poverty),
         lnpop_mean = mean(lnpop),
         bachelor_within = bachelor - bachelor_mean,
         fb_within = fb - fb_mean,
         age65_within = age65 - age65_mean, 
         black_within = black - black_mean,
         rural_within = rural - rural_mean,
         income_within = income - income_mean,
         poverty_within = poverty - poverty_mean,
         lnpop_within = lnpop - lnpop_mean
  ) %>%
  ungroup()
data <- data %>% arrange(GEOID, year)

data$eventid <- seq(nrow(data))
write.csv(data,"Input/demeandataGeorgia.csv",row.names = F)



pdata <- pdata.frame(data, index = c("GEOID", "year"))
model_fe <- plm(bachelor~fb+black+rural+lnpop+income, data = pdata, model = "within", effect = "individual")
summary(model_fe)
fe_values <- fixef(model_fe)
fe_values
vif(model_fe)





################# DISPLAY #################
# maps with significance filtering
font_size <- 6

###### MGWFER #####
shape_data <- st_read("Input/Georgia_Counties/Georgia_Counties.shp")
ggplot(data = shape_data) +
  geom_sf() +
  theme_minimal()
data <- read.csv("Input/demeandataGeorgia.csv", stringsAsFactors = F)
pdata <- pdata.frame(data, index = c("GEOID", "year"))
fe_model <- plm(bachelor ~ lnpop + black + fb + rural + poverty + income, data = pdata, model = "within")
summary(fe_model)
mean(fixef(fe_model))

ai <- read.csv("Output/aiG.csv", stringsAsFactors = F, header = F)
colnames(ai) <- c('ai', 'std', 't_value', 'p_value', 'significance')
parameters <- read.csv("Output/parametersG.csv", stringsAsFactors = F, header = F)
colnames(parameters) <- c("beta_lnpop", "beta_black","beta_fb","beta_rural", "beta_poverty", "beta_income")
sig <- read.csv("Output/sigG.csv", stringsAsFactors = F, header = F)
colnames(sig) <- c("sig_lnpop", "sig_black","sig_fb","sig_rural", "sig_poverty", "sig_income")
mgwfer_results <- cbind(ai, parameters, sig)
scale <- sd(data$bachelor_within)/sapply(data[c('lnpop_within', 'black_within', 'fb_within', 'rural_within', 'poverty_within', 'income_within')], sd)
mgwfer_results$beta_lnpop <- mgwfer_results$beta_lnpop * scale[1]
mgwfer_results$beta_black <- mgwfer_results$beta_black * scale[2]
mgwfer_results$beta_fb <- mgwfer_results$beta_fb * scale[3]
mgwfer_results$beta_rural <- mgwfer_results$beta_rural * scale[4]
mgwfer_results$beta_poverty <- mgwfer_results$beta_poverty * scale[5]
mgwfer_results$beta_income <- mgwfer_results$beta_income * scale[6]
mgwfer_results$GEOID <- as.character(data$GEOID)
mgwfer_results <- mgwfer_results[seq(1, nrow(mgwfer_results), by = 5),]
mgwfer <- left_join(shape_data, mgwfer_results, by = "GEOID")


panel_h_in <- 1.2
bbox <- st_bbox(shape_data)  # use the layer that defines the extent
aspect <- as.numeric((bbox["xmax"] - bbox["xmin"]) / (bbox["ymax"] - bbox["ymin"]))
panel_w_in <- panel_h_in * aspect

filtered_data <- mgwfer %>% filter(significance == 1)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = ai), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-4, 4), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER SC.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)



filtered_data <- mgwfer %>% filter(sig_lnpop != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_lnpop), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(0,8), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER lnpop.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter(sig_fb != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_fb), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER fb.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter(sig_black != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_black), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-1, 1), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER black.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter(sig_rural != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_rural), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER rural.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter(sig_income != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_income), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER income.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter(sig_poverty != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_poverty), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER poverty.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)



###### 2018 MGWR #####
mgwr18 <- read.csv("Output/mgwr18_results.csv", stringsAsFactors = F) # Generated by the MGWR software 
mgwr18$GEOID <- as.character(mgwr18$GEOID)
mgwr18 <- left_join(shape_data, mgwr18, by = "GEOID")


filtered_data <- mgwr18 %>% filter(p_Intercept < 0.05)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_Intercept), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-4, 4), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR SC.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter(p_lnpop < 0.05)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_lnpop), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(0, 8), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR lnpop.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter(p_fb < 0.05)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_fb), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR fb.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter(p_black < 0.05)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_black), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-1, 1), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR black.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter(p_rural < 0.05)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_rural), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR rural.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter(p_income < 0.05)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_income), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR income.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter(p_poverty < 0.05)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_poverty), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR poverty.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


###### Pooled MGWR #####
parameters_pool <- read.csv("Output/parameters_pool_G.csv", stringsAsFactors = F, header = F)
sig_pool <- read.csv("Output/sig_pool_G.csv", stringsAsFactors = F, header = F)
colnames(parameters_pool) <- c("beta_Intercept", "beta_lnpop", "beta_black","beta_fb","beta_rural", "beta_poverty", "beta_income")
colnames(sig_pool) <- c("sig_Intercept", "sig_lnpop", "sig_black","sig_fb","sig_rural", "sig_poverty", "sig_income")
pool_results <- cbind(parameters_pool, sig_pool)
pool_results$GEOID <- as.character(data$GEOID)
pool_results <- pool_results[seq(1, nrow(pool_results), by = 5),]
pool <- left_join(shape_data, pool_results, by = "GEOID")
data <- read.csv("Input/demeandataGeorgia.csv", stringsAsFactors = F)
model <- lm(bachelor ~ lnpop + black + fb + rural + poverty + income, data = data)
summary(model)

filtered_data <- pool %>% filter(sig_Intercept != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_Intercept), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-4, 4), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR SC.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter(sig_lnpop != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_lnpop), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(0, 8), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR lnpop.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter(sig_fb != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_fb), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR fb.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter(sig_black != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_black), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-1, 1), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR black.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter(sig_rural != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_rural), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR rural.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter(sig_income != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_income), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR income.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter(sig_poverty != 0)
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_poverty), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR poverty.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)







################# DISPLAY ALL #################
# maps without significance filtering
font_size <- 6

###### MGWFER #####
shape_data <- st_read("Input/Georgia_Counties/Georgia_Counties.shp")
ggplot(data = shape_data) +
  geom_sf() +
  theme_minimal()
data <- read.csv("Input/demeandataGeorgia.csv", stringsAsFactors = F)
pdata <- pdata.frame(data, index = c("GEOID", "year"))
fe_model <- plm(bachelor ~ lnpop + black + fb + rural + poverty + income, data = pdata, model = "within")
summary(fe_model)
mean(fixef(fe_model))

ai <- read.csv("Output/aiG.csv", stringsAsFactors = F, header = F)
colnames(ai) <- c('ai', 'std', 't_value', 'p_value', 'significance')
parameters <- read.csv("Output/parametersG.csv", stringsAsFactors = F, header = F)
colnames(parameters) <- c("beta_lnpop", "beta_black","beta_fb","beta_rural", "beta_poverty", "beta_income")
sig <- read.csv("Output/sigG.csv", stringsAsFactors = F, header = F)
colnames(sig) <- c("sig_lnpop", "sig_black","sig_fb","sig_rural", "sig_poverty", "sig_income")
mgwfer_results <- cbind(ai, parameters, sig)
scale <- sd(data$bachelor_within)/sapply(data[c('lnpop_within', 'black_within', 'fb_within', 'rural_within', 'poverty_within', 'income_within')], sd)
mgwfer_results$beta_lnpop <- mgwfer_results$beta_lnpop * scale[1]
mgwfer_results$beta_black <- mgwfer_results$beta_black * scale[2]
mgwfer_results$beta_fb <- mgwfer_results$beta_fb * scale[3]
mgwfer_results$beta_rural <- mgwfer_results$beta_rural * scale[4]
mgwfer_results$beta_poverty <- mgwfer_results$beta_poverty * scale[5]
mgwfer_results$beta_income <- mgwfer_results$beta_income * scale[6]
mgwfer_results$GEOID <- as.character(data$GEOID)
mgwfer_results <- mgwfer_results[seq(1, nrow(mgwfer_results), by = 5),]
mgwfer <- left_join(shape_data, mgwfer_results, by = "GEOID")


panel_h_in <- 1.2
bbox <- st_bbox(shape_data)  # use the layer that defines the extent
aspect <- as.numeric((bbox["xmax"] - bbox["xmin"]) / (bbox["ymax"] - bbox["ymin"]))
panel_w_in <- panel_h_in * aspect

filtered_data <- mgwfer %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = ai), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-4, 4), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER SC1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)



filtered_data <- mgwfer %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_lnpop), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(0,8), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER lnpop1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_fb), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER fb1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_black), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-1, 1), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER black1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_rural), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER rural1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_income), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER income1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwfer %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_poverty), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWFER poverty1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)



###### 2018 MGWR #####
mgwr18 <- read.csv("Output/mgwr18_results.csv", stringsAsFactors = F)
mgwr18$GEOID <- as.character(mgwr18$GEOID)
mgwr18 <- left_join(shape_data, mgwr18, by = "GEOID")


filtered_data <- mgwr18 %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_Intercept), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-4, 4), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR SC1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_lnpop), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(0, 8), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR lnpop1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_fb), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR fb1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_black), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-1, 1), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR black1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_rural), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR rural1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_income), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR income1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- mgwr18 %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_poverty), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/MGWR poverty1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


###### Pooled MGWR #####
parameters_pool <- read.csv("Output/parameters_pool_G.csv", stringsAsFactors = F, header = F)
sig_pool <- read.csv("Output/sig_pool_G.csv", stringsAsFactors = F, header = F)
colnames(parameters_pool) <- c("beta_Intercept", "beta_lnpop", "beta_black","beta_fb","beta_rural", "beta_poverty", "beta_income")
colnames(sig_pool) <- c("sig_Intercept", "sig_lnpop", "sig_black","sig_fb","sig_rural", "sig_poverty", "sig_income")
pool_results <- cbind(parameters_pool, sig_pool)
pool_results$GEOID <- as.character(data$GEOID)
pool_results <- pool_results[seq(1, nrow(pool_results), by = 5),]
pool <- left_join(shape_data, pool_results, by = "GEOID")
data <- read.csv("Input/demeandataGeorgia.csv", stringsAsFactors = F)
model <- lm(bachelor ~ lnpop + black + fb + rural + poverty + income, data = data)
summary(model)

filtered_data <- pool %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_Intercept), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-4, 4), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR SC1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_lnpop), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(0, 8), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR lnpop1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_fb), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR fb1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_black), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-1, 1), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR black1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_rural), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR rural1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_income), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR income1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)


filtered_data <- pool %>% filter()
p <- ggplot() + geom_sf(data = shape_data, fill = "white", color = "black", linewidth = 0.05, lineend = "butt", linejoin = "mitre") + geom_sf(data = filtered_data, aes(fill = beta_poverty), color = "black", linewidth = 0.03, lineend = "butt", linejoin = "mitre") + scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, limits = c(-0.5, 0.5), oob = scales::squish) + labs(fill = "Value") + coord_sf(expand = FALSE) + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.title = element_text(size = font_size + 1), legend.text  = element_text(size = font_size, hjust = 1), legend.key.height = unit(0.2, "cm"), legend.key.width  = unit(0.2, "cm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(0, 0, 0, 0, "cm"))
p_tight <- ggdraw(p) + theme(plot.margin = margin(0,0,0,0,"pt"))
save_plot("pictures/PMGWR poverty1.tiff", p_tight, base_height = 1.2, base_width = 1.5, dpi = 600)

