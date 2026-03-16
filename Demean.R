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
library(pracma)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



### sc->x1,sc->x2,x1->x2,sc->x4 ###
set.seed(8) #5 
n <- 30
i <- rep(seq(1, n), each = n)
j <- rep(seq(1, n), times = n)
pre <- data.frame(i = i, j = j)
pre$id <- seq(n**2)
pre$sc <- 30*(exp((pre$j)/n)-1) # 30 controls the bias scale, especially for the local bias of x4

pre$bx1 <- 1+1*(ceiling(n/4)**2-(ceiling(n/4)-pre$i/2)**2)*(ceiling(n/4)**2-(ceiling(n/4)-pre$j/2)**2)/(ceiling(n/4)**2)**2
pre$bx2 <- 1+1*(pre$i + pre$j)/(n+n)
pre$bx3 <- 1+0.5
pre$year <- 1

mid <- pre
mid$year <- 2

post <- pre
post$year <- 3

data <- rbind(pre, mid, post)
data$eventid <- seq(3*n**2)

data$x1 <- rnorm(3*n**2, mean = 0, sd = 0.5) + 0.05*data$sc
data$x2 <- rnorm(3*n**2, mean = 0, sd = 0.5) + 0.05*data$sc 
data$x3 <- rnorm(3*n**2, mean = 0, sd = 0.5) + 0.05*data$sc 
data$x4 <- rnorm(3*n**2, mean = 0, sd = 0.5) + 0.05*data$sc 
data$error <- rnorm(3*n**2, mean = 0, sd = 0.5)
data$y <- data$error + data$sc + data$bx1*data$x1 +  data$bx2*data$x2 + data$bx3*data$x3

data$ratio <- data$y / data$sc


model <- lm(y ~ x1 + x2 + x3 + x4, data)
summary(model)
vif(model)

mid <- data[data$year==2,]
model <- lm(y ~ x1 + x2 + x3 + x4 , mid)
summary(model)
vif(model)

pdata <- pdata.frame(data, index = c("id", "year"))
fe_model <- plm(y ~ x1 + x2 + x3 + x4, data = pdata, model = "within")
summary(fe_model)
mean(fixef(fe_model))

data <- data %>%
  group_by(id) %>%
  mutate(
    y_mean = mean(y),
    x1_mean = mean(x1),
    x2_mean = mean(x2),
    x3_mean = mean(x3),
    x4_mean = mean(x4),
    y_within = y - y_mean,
    x1_within = x1 - x1_mean,
    x2_within = x2 - x2_mean,
    x3_within = x3 - x3_mean,
    x4_within = x4 - x4_mean
  ) %>%
  ungroup()
data <- data %>% arrange(id, year)

write.csv(data, "Input/Pscenario.csv", row.names = F)
write.csv(mid,"Input/mid.csv", row.names = F)
hist(data$ratio)





################# DISPLAY #################

################# TRUE #################
font_size = 6
ggplot(mid, aes(x = j, y = i, fill = sc)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-20, -15, 0,25, 50)), limits = c(-20, 50), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "True SC", x = "U",y = "V", fill = "Value")  + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1) 
ggsave("pictures/True SC.tiff", width = 3.2,  height = 2.2, dpi = 600)


ggplot(mid, aes(x = j, y = i, fill = bx1)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd", "gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "True Beta_X1", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/True Beta_X1.tiff", width = 3.2,  height = 2.2, dpi = 600)

ggplot(mid, aes(x = j, y = i, fill = bx2)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "True Beta_X2", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/True Beta_X2.tiff", width = 3.2,  height = 2.2, dpi = 600)

ggplot(mid, aes(x = j, y = i, fill = bx3)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "True Beta_X3", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/True Beta_X3.tiff", width = 3.2,  height = 2.2, dpi = 600)

ggplot(mid, aes(x = j, y = i)) +
  geom_tile(fill = "white", color = "black") +  # White tiles with light gray borders
  theme_minimal(base_family = "Arial") +
  labs(title = "True Beta_X4", x = "U", y = "V", fill = NULL) + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/True Beta_X4.tiff", width = 3.2,  height = 2.2, dpi = 600)

################# CROSS-SECTIONAL #################
mid <-  read.csv("Input/mid.csv", stringsAsFactors = F)
mgwr <- read.csv("Output/mid_results.csv", stringsAsFactors = F)  # Generated by the MGWR software  
scale <- sd(mid$y)/c(1, sapply(mid[c('x1', 'x2', 'x3', 'x4')], sd))
mgwr$scale_Intercept <- mgwr$beta_Intercept * scale[1]
mgwr$scale_x1 <- mgwr$beta_x1 * scale[2]
mgwr$scale_x2 <- mgwr$beta_x2 * scale[3]
mgwr$scale_x3 <- mgwr$beta_x3 * scale[4]
mgwr$scale_x4 <- mgwr$beta_x4 * scale[5]

filtered <- mgwr %>% filter(p_Intercept < 0.05)
ggplot(filtered, aes(x = y_coor, y = x_coor, fill = scale_Intercept)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-20, -15, 0,25, 50)), limits = c(-20, 50), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWR SC", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWR SC.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- mgwr %>% filter(p_x1 < 0.05)
ggplot(filtered, aes(x = y_coor, y = x_coor, fill = scale_x1)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWR Beta_X1", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWR beta_X1.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- mgwr %>% filter(p_x2 < 0.05)
ggplot(filtered, aes(x = y_coor, y = x_coor, fill = scale_x2)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWR Beta_X2", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWR beta_X2.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- mgwr %>% filter(p_x3 < 0.05)
ggplot(filtered, aes(x = y_coor, y = x_coor, fill = scale_x3)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWR Beta_X3", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWR beta_X3.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- mgwr %>% filter(p_x4 < 0.05)
ggplot(filtered, aes(x = y_coor, y = x_coor, fill = scale_x4)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(-2, 2), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWR Beta_X4", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWR beta_X4.tiff", width = 3.2,  height = 2.2, dpi = 600)




################# POOLED #################
data <- read.csv("Input/Pscenario.csv", stringsAsFactors = F)
pool_parameters <- read.csv("Output/parameters_pool.csv", stringsAsFactors = F, header = F)
pool_sig <- read.csv("Output/sig_pool.csv", stringsAsFactors = F, header = F)
colnames(pool_parameters) <- c('beta_Intercept', 'beta_x1', 'beta_x2', 'beta_x3', 'beta_x4')
colnames(pool_sig) <- c('t_Intercept', 't_x1', 't_x2', 't_x3', 't_x4')
pool <- cbind(pool_parameters, pool_sig)
pool <-pool[seq(1, nrow(pool), by = 3),]
pool$i <- mid$i
pool$j <- mid$j
scale <- sd(data$y)/c(1, sapply(data[c('x1', 'x2', 'x3', 'x4')], sd))
pool$scale_Intercept <- pool$beta_Intercept * scale[1]
pool$scale_x1 <- pool$beta_x1 * scale[2]
pool$scale_x2 <- pool$beta_x2 * scale[3]
pool$scale_x3 <- pool$beta_x3 * scale[4]
pool$scale_x4 <- pool$beta_x4 * scale[5]

filtered <- pool %>% filter(t_Intercept != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_Intercept)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-20, -15, 0,25, 50)), limits = c(-20, 50), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "PMGWR SC", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/Pooled MGWR SC.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- pool %>% filter(t_x1 != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_x1)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "Pooled MGWR Beta_X1", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/Pooled MGWR beta_X1.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- pool %>% filter(t_x2 != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_x2)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "Pooled MGWR Beta_X2", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/Pooled MGWR beta_X2.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- pool %>% filter(t_x3 != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_x3)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "Pooled MGWR Beta_X3", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/Pooled MGWR beta_X3.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- pool %>% filter(t_x4 != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_x4)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(-2, 2), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "Pooled MGWR Beta_X4", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/Pooled MGWR beta_X4.tiff", width = 3.2,  height = 2.2, dpi = 600)


################# PANEL #################
ai <- read.csv("Output/ai.csv", stringsAsFactors = F)
ai <- ai[seq(1, nrow(ai), by = 3),]
ai$i <- pre$i
ai$j <- pre$j
filtered <- ai %>% filter(significance == 1)
ggplot(filtered, aes(x = j, y = i, fill = ai)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-20, -15, 0,25, 50)), limits = c(-20, 50), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWFER SC", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWFER SC.tiff", width = 3.2,  height = 2.2, dpi = 600)


panel_parameters <- read.csv("Output/parameters.csv", stringsAsFactors = F, header = F)
panel_sig <- read.csv("Output/sig.csv", stringsAsFactors = F, header = F)
colnames(panel_parameters) <- c('beta_x1', 'beta_x2', 'beta_x3', 'beta_x4')
colnames(panel_sig) <- c('t_x1', 't_x2', 't_x3', 't_x4')
panel <- cbind(panel_parameters, panel_sig)
panel <-panel[seq(1, nrow(panel), by = 3),]
panel$i <- mid$i
panel$j <- mid$j
scale <- sd(data$y_within)/sapply(data[c('x1_within', 'x2_within', 'x3_within', 'x4_within')], sd)
panel$scale_x1 <- panel$beta_x1 * scale[1]
panel$scale_x2 <- panel$beta_x2 * scale[2]
panel$scale_x3 <- panel$beta_x3 * scale[3]
panel$scale_x4 <- panel$beta_x4 * scale[4]

filtered <- panel %>% filter(t_x1 != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_x1)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWFER Beta_X1", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWFER beta_X1.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- panel %>% filter(t_x2 != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_x2)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWFER Beta_X2", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWFER beta_X2.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- panel %>% filter(t_x3 != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_x3)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(1, 3), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWFER Beta_X3", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWFER beta_X3.tiff", width = 3.2,  height = 2.2, dpi = 600)

filtered <- panel %>% filter(t_x4 != 0)
ggplot(filtered, aes(x = j, y = i, fill = scale_x4)) +
  geom_tile() +                # Create the grid with tiles
  scale_fill_gradientn(colours = c("#2b83ba", "#3288bd","gray", "#f46d43", "#d7191c"), values = scales::rescale(c(-600, -400, 0, 400, 600)), limits = c(-2, 2), oob = scales::squish) + # Color scale
  theme_minimal(base_family = "Arial") +            # Clean theme
  labs(title = "MGWFER Beta_X4", x = "U",y = "V", fill = "Value") + scale_x_continuous(limits = c(0, 31), expand = c(0, 0)) + scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) + theme(   axis.title.x = element_text(size = font_size + 1),   axis.title.y = element_text(size = font_size + 1),   axis.text    = element_text(size = font_size),   plot.title   = element_text(size = font_size + 1),   legend.position   = "right",   legend.direction  = "vertical",   legend.box.margin = margin(0, 0, 0, 0),   legend.spacing.x  = unit(2, "pt"),   legend.spacing.y  = unit(2, "pt"),   legend.key.width  = unit(0.3, "cm"),   legend.title      = element_text(size = font_size + 1),   legend.text       = element_text(size = font_size) ) + guides(   fill = guide_colorbar(     barwidth  = unit(3,  "mm"),     barheight = unit(18, "mm"),     ticks = FALSE   ) ) + coord_fixed(ratio = 1)
ggsave("pictures/MGWFER beta_X4.tiff", width = 3.2,  height = 2.2, dpi = 600)



################# END #################








