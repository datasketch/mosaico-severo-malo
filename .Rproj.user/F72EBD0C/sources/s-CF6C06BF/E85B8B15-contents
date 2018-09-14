
library(tidyverse)
library(magick)

# http://livefreeordichotomize.com/2017/07/18/the-making-of-we-r-ladies/

srcimgs1 <- list.files("src_img_ori/corrupcionario", full.names = TRUE, recursive = TRUE,
                      pattern = "\\.jpg")
srcimgs2 <- list.files("src_img_ori/img_data", full.names = TRUE, recursive = TRUE,
                      pattern = "\\.jpg")
srcimgs <- c(rep(srcimgs1,3),srcimgs2)
## Rescale images
map(seq_along(srcimgs), function(i){
  img <- image_read(srcimgs[i])
  img <- img %>% image_scale("500x500!")
  if(runif(1) < .3){
    img <- img %>% image_negate()
  }
  tone <- sample(LETTERS[1:6],1)
  img %>% image_write(path = paste0("srcimgs/",i,".jpeg"), format = "jpeg")
})

## Resize target img
#target <- "target.png"
target <- "target-cluster.jpeg"

#image_read(target) %>% image_write(path = "target.jpeg", format = "jpg")


target_mosaic <- "target-mosaic.jpeg"
image_read(target) %>% image_scale("100x100!") %>%
  image_write(path = target_mosaic, format = "jpeg")


srcimgs <- list.files("srcimgs", full.names = TRUE, pattern = "\\.jpeg")

# Make the mosaic

library(RsimMosaic)
composeMosaicFromImageRandom(
  target_mosaic,
  "mosaic.jpg",
  "srcimgs",
  removeTiles=TRUE
)


