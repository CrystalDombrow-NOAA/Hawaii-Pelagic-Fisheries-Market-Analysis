library(wesanderson)
library(cowplot)
library(scales)

names(wes_palettes)
library(RColorBrewer)

WesColChange <- function(colPalette = NULL) {
  if (missing(colPalette)) {
    ctr <- 1
    while (ctr <= 25) {
      assign(paste("wescol", ctr, sep = ""),
        wes_palette("Rushmore", ctr, type = "continuous"),
        envir = globalenv())
      ctr <- ctr + 1}
  } else {
    ctr <- 1
    while (ctr <= 25) {
      assign(paste("wescol", ctr, sep = ""),
        wes_palette(colPalette, ctr, type = "continuous"),
        envir = globalenv())
      ctr <- ctr + 1}}}

WesColChange()

blgncols <- brewer.pal(9, "BuGn")

pie(rep(1, 9), col = blgncols)

newcol <- colorRampPalette(blgncols)

ncols <- 10

#Apply the function to get 100 colors
blgncols2 <- newcol(ncols) 

ctr <- 1
while (ctr <= 10) {
  assign(paste("colBrew", ctr, sep = ""), newcol(ctr))
  ctr <- ctr + 1}

n <- 25

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]

col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, 
                            rownames(qual_col_pals)))

pie(rep(1, n), col = sample(col_vector, n))