library(magick)
library(MetBrewer)
img <- image_read("images/final_cali__shadow6.png")
hg <- met.brewer("VanGogh1")
swatchplot(hg)

img %>% 
  image_crop(gravity = "east",
             geometry = "4500x3200") %>% 
  image_annotate("California's Population by Density", 
                 gravity = "north",
                 location = "+300+300",
                 size = 250,
                 font = "Times New Roman",
                 color = hg[2]) %>% 
  image_annotate("California's population is 
concentrated along its
coast in several major hubs", 
                 gravity = "east",
                 location = "+200+200",
                 size = 165,
                 font = "Times New Roman",
                 color = hg[1]) %>%
  image_write("images/final_plot_cali.png")
