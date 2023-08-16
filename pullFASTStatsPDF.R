# install packages for pdf text work
install.packages("pdftools", INSTALL_opts = "--no-multiarch")
install.packages("tesseract", INSTALL_opts = "--no-multiarch")

pacman::p_load(here, lubridate, tidyverse, readr, janitor, data.table, pdftools, tesseract)
here()
wd <- here()

pngfile <- pdftools::pdf_convert("C://Users/vegas/Desktop/UConn/researchProjects/NeckMobility/data/pro/fast/pdf/ngu/Daniel1.pdf", dpi = 600)  

images <- magick::image_read(pngfile) 

images <- images %>%
  magick::image_scale("x2000") %>%                        # rescale
  magick::image_background("white", flatten = TRUE) %>%   # set background to white
  magick::image_trim() %>%                                # Trim edges that are the background color from the image.
  magick::image_noise() %>%                               # Reduce noise in image using a noise peak elimination filter
  magick::image_enhance() %>%                             # Enhance image (minimize noise)
  magick::image_normalize() %>%                           # Normalize image (increase contrast by normalizing the pixel values to span the full range of color values).
  magick::image_contrast(sharpen = 1) %>%                 # increase contrast
  magick::image_deskew(threshold = 40)                     # deskew image -> creates negative offset in some scans

magick::image_browse(images)
text <- tesseract::ocr(images)



text_list <- text %>% 
  map(., ~str_split(., "\n"))

head(text_list)