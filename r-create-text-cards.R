library(magick)
library(tidyverse)
source("helpers.R")

frases <- read_csv("data/corrupcionario.csv")
frases <- mop::na_to_empty_chr(frases,empty = " ")

#available_colors <- c("#FA5439", "#DA4589","#5498A0","#45DCC0")
available_colors <- read_csv("color_dist.csv")

#bg_color <- "#FA5439"
#create_bg_img(bg_color)
#contrast_color(bg_color)

i <<- 1
walk(transpose(frases), function(f){
  #f <- transpose(frases)[[1]]
  message(f[[1]], f[[2]])
  text1 <- wrap_sentence(f[[1]], 12)
  text2 <- wrap_sentence(f[[2]],23)
  text3 <- wrap_sentence(f[[3]],28)
  #bg_color <- "#FA5439"
  bg_color <- sample(available_colors$color,1,prob = available_colors$prop)
  bg <- create_bg_img(bg_color)
  text_color <- contrast_color(bg_color)
  img <- bg %>% 
    image_annotate(text1, size = 40, color = text_color, #font = 'Press Start 2P',
                          degrees = 0, location = "+25+10") %>% 
    image_annotate(text2, size = 30, color = text_color, #font = 'Press Start 2P',
                   degrees = 0, location = "+25+150") %>% 
  image_annotate(text3, size = 20, color = text_color, #font = 'Press Start 2P',
                 degrees = 0, location = "+25+400") 
  image_write(img, paste0("src_img_ori/corrupcionario/",i,".jpg"))
  i <<- i+1
})




