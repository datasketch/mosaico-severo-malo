
library(magick)


img <- image_read("target.jpeg")
img_df <- imgarr_to_df(img)

img_cls <- img_kmeans(img, clusters = 5)
image_write(img_cls$img,"target-cluster.jpeg")
img_cls$colors_dist

color_dist <- data_frame(
  color = names(img_cls$colors_dist),
  prop = img_cls$colors_dist

)
write_csv(color_dist,"color_dist.csv")


### QUANTIZE
#imgq <- image_quantize(img, 5)
### CREATE PALETTE
# library(paletter)
# x <- create_palette(image_path = "target.jpeg",
#                number_of_colors =10,
#                type_of_variable = "categorical")
# show_col(x)
# write_csv(data_frame(color = x),"palette.csv")
