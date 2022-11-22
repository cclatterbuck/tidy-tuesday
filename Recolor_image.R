library(magick)
library(here)

sink <-image_transparent(image_read("https://raw.githubusercontent.com/cclatterbuck/tidy-tuesday/master/images/sinkingship.png"), 'white')
sink <- image_colorize(sink, 100, "#FF9933")
image_write(sink, path = here("images", "sinkingship_color.png"), format = "png")
            
sail <-image_transparent(image_read("https://raw.githubusercontent.com/cclatterbuck/tidy-tuesday/master/images/sailingship.png"), 'white')
sail <- image_colorize(sail, 100, "#000099")
image_write(sail, path = here("images", "sailingship_color.png"), format = "png")          
