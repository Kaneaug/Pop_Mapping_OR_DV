library(magick)
library(MetBrewer)
img <- image_read("images/draft_plot4.5.png")
hg <- met.brewer("Pillement")
swatchplot(hg)

img %>% 
  image_crop(gravity = "center",
             geometry = "6000x3800") %>% 
  image_annotate("Oregon Population by Density", 
                 gravity = "north",
                 location = "+200+200",
                 size = 250,
                 font = "Times New Roman",
                 color = hg[2]) %>% 
  image_annotate("Oregon population is concentrated along the I-5 corridor that spans through the western quarter
of the state running through the Willamette Valley with the main metro areas consisting of 
Portland, Salem, and Eugene", 
                 gravity = "southwest",
                 location = "+200+200",
                 size = 135,
                 font = "Times New Roman",
                 color = hg[5]) %>%
  image_write("images/final_plot4.5_captioned.png")
