library(ggmagic)

# tema

theme_black <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "none",
      panel.background = element_rect(fill = "black", color  =  NA),
      panel.grid.major = element_line(color = "grey10"),
      panel.grid.minor = element_line(color = "grey10"),
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      )
}



# gráficas

colr <- c("#02BD74", "#BA9D00", "#00ADFB","#8BAB00", "#ED6F69", "#F959DA", "#E18A00", "#28b605", "#9D91EA")


# Corruption events by affected sector BIEN BIEN BIEN
title <- "Corruption events by affected sectors"
p0 <- data_frame("Afected sector" = c("Education", "Health", "Infrastructure",
                                      "Internal public\nmanagement", "Electoral",
                                      "Public services and\nsanitation", "Industry and\ncommerce",
                                      "Land", "Justice", "National security", "Armed conflict",
                                      "Miner -\nEnergetic", "Technology and\ninnovation"),
                 "Number of cases" = c(35, 26, 22, 19, 18, 14, 8, 8, 6, 6, 4, 4, 3))

gg_bar_CatNum(p0, title = title, orientation = "hor", sort = "desc", colors = colr) + 
  ggsave("src_img_original_data/g0.jpg", height = 150, width = 150, units = "mm")
gg_bar_CatNum(p0, title = title, orientation = "hor", sort = "desc", colors = colr, colorText = "white") + 
  theme_black() +
  ggsave("src_img_original_data/g1.jpg", height = 150, width = 150, units = "mm")

p0$lab <- paste0(p0$`Afected sector`, "\n", p0$`Number of cases`)
ggplot(p0, aes(area = `Number of cases`, fill = `Afected sector`, label = lab)) +
  geom_treemap() +
  geom_treemap_text(colour = "black", place = "centre") +
  theme_ds() +
  theme(legend.position = "none") +
  labs(title = paste0(title, "\n")) +
  ggsave("src_img_original_data/g2.jpg", height = 150, width = 150, units = "mm")

ggplot(p0, aes(area = `Number of cases`, fill = `Afected sector`, label = lab)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre") +
  theme_ds() +
  theme(legend.position = "none") +
  labs(title = paste0(title, "\n")) +
  theme_black() +
  ggsave("src_img_original_data/g3.jpg", height = 150, width = 150, units = "mm")

# Corruption events by department    
title <- "Corruption events by department"
p1 <- data_frame("Department" = c("Guajira", "Nariño", "Valle", "Antioquia", 
                                  "Chocó", "Bolívar", "Cesar", "Caquetá", 
                                  "Putumayo", "Guaviare", "Sucre", "Cauca"),
                 "Number of cases" = c(18, 18, 18, 17, 17, 16, 14, 10, 9, 8, 8, 7))

gg_bar_CatNum(p1, title = title, orientation = "hor", sort = "desc", colors = colr) +
  ggsave("src_img_original_data/g4.jpg", height = 150, width = 150, units = "mm")
gg_bar_CatNum(p1, title = title, orientation = "hor", sort = "desc", colors = colr, colorText = "white") +
  theme_black() +
  ggsave("src_img_original_data/g5.jpg", height = 150, width = 150, units = "mm")


# Corruption events by types BIEN BIEN BIEN
title <- "Corruption events by type"
p2 <- data_frame("Type" = c("Administrative\ncorruption",  "Political\ncorruption",
                            "State\ncapture", "Private\ncorruption"),
                 "Number of cases" = c(150, 21, 8, 7))

gg_bar_CatNum(p2, title = title, sort = "desc", colors = colr) +
  ggsave("src_img_original_data/g7.jpg", height = 150, width = 150, units = "mm")
gg_bar_CatNum(p2, title = title, sort = "desc", colors = colr, colorText = "white") +
  theme_black() +
  ggsave("src_img_original_data/g8.jpg", height = 150, width = 150, units = "mm")

p2$lab <- paste0(p2$Type, "\n", p2$`Number of cases`)

ggplot(p2, aes(area = `Number of cases`, fill = Type, label = lab)) +
  geom_treemap() +
  geom_treemap_text(colour = "black", place = "centre") +
  theme_ds() +
  theme(legend.position = "none") +
  labs(title = paste0(title, "\n")) +
  ggsave("src_img_original_data/g9.jpg", height = 150, width = 150, units = "mm")

ggplot(p2, aes(area = `Number of cases`, fill = Type, label = lab)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre") +
  theme_ds() +
  theme(legend.position = "none") +
  labs(title = paste0(title, "\n")) +
  theme_black() +
  ggsave("src_img_original_data/g10.jpg", height = 150, width = 150, units = "mm")

# Corruption events by nature
title <- "Corruption events by nature"
p3 <- data_frame("Nature" = c("Public\ncontracting",  "Administrative\nprocesses",
                              "Tax\nmanagement"),
                 "Number of cases" = c(87, 44, 15))

gg_bar_CatNum(p3, title = title, colors = colr, orientation = "hor") +
  ggsave("src_img_original_data/g11.jpg", height = 150, width = 150, units = "mm")
gg_bar_CatNum(p3, title = title, colors = colr, orientation = "hor", colorText = "white") +
  theme_black() +
  ggsave("src_img_original_data/g12.jpg", height = 150, width = 150, units = "mm")

p3$lab <- paste0(p3$Nature, "\n", p3$`Number of cases`)
ggplot(p3, aes(area = `Number of cases`, fill = Nature, label = lab)) +
  geom_treemap() +
  geom_treemap_text(colour = "black", place = "centre") +
  theme_ds() +
  theme(legend.position = "none") +
  labs(title = paste0(title, "\n")) +
  ggsave("src_img_original_data/g13.jpg", height = 150, width = 150, units = "mm")

ggplot(p3, aes(area = `Number of cases`, fill = Nature, label = lab)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre") +
  theme_ds() +
  theme(legend.position = "none") +
  labs(title = paste0(title, "\n")) +
  theme_black() +
  ggsave("src_img_original_data/g14.jpg", height = 150, width = 150, units = "mm")


# éstos NA son cero porque no hay cuenta????¿¿¿¿
title <- "Corruption events by type and years"
p4 <- data_frame("Type" = c(rep("Administrative\ncorruption", 20),
                            rep("State capture", 20), 
                            rep("Political\ncorruption", 20),
                            rep("Private\ncorruption", 20)),
                 "Year" = rep(1997:2016, 4),
                 "Number of cases" = c(1, NA, 1, 1, 1, 3, 3, 4, 8, 8, 13, 
                                       16, 11, 14, 14, 15, 14, 9, 12, 2,
                                       1, NA, NA, 1, NA, 1, NA, NA, NA, NA,
                                       NA, 2, 1, 1, NA, 1, NA, NA, NA, NA,
                                       NA, NA, NA, 1, 1, 1, 1, NA, NA, NA, 
                                       NA, 1, NA, 1, 3, NA, NA, 2, 10, NA,
                                       NA, NA, 1, NA, NA, NA, NA, NA, NA, NA,
                                       NA, NA, 1, NA, NA, NA, 1, 2, 1, 1))

gg_line_point_multi_CatCatNum(p4, titleLabel = title, leg_pos = "bottom", yLabel = "Number of cases") + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g15.jpg", height = 200, width = 200, units = "mm")

gg_line_point_multi_CatCatNum(p4, titleLabel = title, leg_pos = "bottom", yLabel = "Number of cases") + 
  theme_black() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key = element_rect(color = "black")) +
  ggsave("src_img_original_data/g16.jpg", height = 200, width = 200, units = "mm")

gg_area_stacked_100_hor_CatCatNum.(p4[, c(2, 1, 3)], titleLabel = title, leg_pos = "bottom", yLabel = "Number of cases (%)") +
theme(legend.title = element_blank(), 
      legend.position = "bottom",
      axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g17.jpg", height = 150, width = 150, units = "mm")

gg_area_stacked_100_hor_CatCatNum.(p4[, c(2, 1, 3)], titleLabel = title, leg_pos = "bottom", yLabel = "Number of cases (%)") +
  theme_black() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key = element_rect(color = "black")) +
  ggsave("src_img_original_data/g18.jpg", height = 150, width = 150, units = "mm")

gg_area_stacked_ver_CatCatNum(p4[, c(2, 1, 3)], titleLabel = title, leg_pos = "bottom",  yLabel = "Number of cases") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g19.jpg", height = 200, width = 200, units = "mm")

gg_area_stacked_ver_CatCatNum(p4[, c(2, 1, 3)], titleLabel = title, leg_pos = "bottom",  yLabel = "Number of cases") +
  theme_black() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key = element_rect(color = "black")) +
  ggsave("src_img_original_data/g20.jpg", height = 200, width = 200, units = "mm")

gg_bar_stacked_100_ver_CatCatNum(p4[, c(2, 1, 3)], titleLabel = title,  yLabel = "Number of cases (%)", leg_pos = "bottom") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g21.jpg", height = 200, width = 200, units = "mm")

gg_bar_stacked_100_ver_CatCatNum(p4[, c(2, 1, 3)], titleLabel = title,  yLabel = "Number of cases (%)", leg_pos = "bottom") +
  theme_black() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(color = "black"),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g22.jpg", height = 200, width = 200, units = "mm")

gg_bar_stacked_hor_CatCatNum(p4[, c(2, 1, 3)], leg_pos = "bottom", titleLabel = title,  yLabel = "Number of cases") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g23.jpg", height = 200, width = 200, units = "mm")

gg_bar_stacked_hor_CatCatNum(p4[, c(2, 1, 3)], leg_pos = "bottom", titleLabel = title,  yLabel = "Number of cases", color_text = "white") +
  theme_black() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key = element_rect(color = "black")) +
  ggsave("src_img_original_data/g24.jpg", height = 200, width = 200, units = "mm")
  
###
title <- "Corruption events by nature and departments"
p5 <- data_frame("Department" = rep(c("Antioquia", "Arauca", "Bolívar", "Caquetá",
                                      "Cauca", "Cesar", "Chocó", "Córdoba",
                                      "Guajira", "Guaviare", "Meta", "Nariño",
                                      "Putumayo", "Sucre", "Tolima", "Valle"), 3),
                 "Nature" = c(rep("Public contracting", 16),
                              rep("Tax management", 16),
                              rep("Administrative processes", 16)),
                 "Number of cases" = c(7, NA, 6,
                                       1, 1, 2,
                                       8, NA, 4,
                                       6, 1, 2,
                                       3, 1, 1,
                                       8, 1, 4,
                                       9, NA, 4,
                                       3, 2, NA,
                                       10, 1, 5,
                                       4, NA, NA, 
                                       2, 1, NA, 
                                       2, 4, 4,
                                       5, NA, 2,
                                       4, 1, 2,
                                       4, NA, 1,
                                       9, 2, 5))


gg_area_stacked_100_ver_CatCatNum.(p5, titleLabel = title, leg_pos = "bottom", yLabel = "Number of cases (%)") +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g25.jpg", height = 250, width = 250, units = "mm")

gg_area_stacked_100_ver_CatCatNum.(p5, titleLabel = title, leg_pos = "bottom", yLabel = "Number of cases (%)") +
  theme_black() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key = element_rect(color = "black")) +
  ggsave("src_img_original_data/g26.jpg", height = 250, width = 250, units = "mm")

gg_area_stacked_ver_CatCatNum(p5, titleLabel = title, leg_pos = "bottom",  yLabel = "Number of cases") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g27.jpg", height = 250, width = 250, units = "mm")

gg_area_stacked_ver_CatCatNum(p5, titleLabel = title, leg_pos = "bottom",  yLabel = "Number of cases") +
  theme_black() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key = element_rect(color = "black")) +
  ggsave("src_img_original_data/g28.jpg", height = 250, width = 250, units = "mm")

gg_bar_stacked_100_hor_CatCatNum(p5, titleLabel = title,  yLabel = "Number of cases (%)", leg_pos = "bottom") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g29.jpg", height = 200, width = 200, units = "mm")

gg_bar_stacked_100_hor_CatCatNum(p5, titleLabel = title,  yLabel = "Number of cases (%)", leg_pos = "bottom") +
  theme_black() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(color = "black"),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g30.jpg", height = 200, width = 200, units = "mm")

gg_bar_stacked_ver_CatCatNum(p5, leg_pos = "bottom", titleLabel = title,  yLabel = "Number of cases") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5)) +
  ggsave("src_img_original_data/g31.jpg", height = 250, width = 250, units = "mm")

gg_bar_stacked_ver_CatCatNum(p5, leg_pos = "bottom", titleLabel = title,  yLabel = "Number of cases", color_text = "white") +
  theme_black() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key = element_rect(color = "black")) +
  ggsave("src_img_original_data/g32.jpg", height = 250, width = 250, units = "mm")

## MAPA
library(geodata)

q1 <- data_frame("Department" = c("Guajira", "Nariño", "Valle", "Antioquia",
                                  "Chocó", "Bolívar", "Cesar", "Caquetá",
                                  "Putumayo", "Guaviare", "Sucre", "Cauca",
                                  "Tolima", "Córdoba", "Norte de santander",
                                  "Arauca"),
                 "Cod" = c(44, 52, 76, 05, 27, 13, 20, 18, 86, 95, 70, 19, 73, 23, 54, 81),
                 "Number of cases" = c(18, 18, 18, 17, 17, 16, 14, 10, 9, 8, 8, 7, 6, 6, 5, 6))

geomagic::gg_choropleth_map_GcdNum.(q1[, 2:3], mapName = "col_departments", opts = list(highC = colr[5], lowC = colr[1])) +
  ggsave("src_img_original_data/g6.jpg", height = 150, width = 150, units = "mm")
