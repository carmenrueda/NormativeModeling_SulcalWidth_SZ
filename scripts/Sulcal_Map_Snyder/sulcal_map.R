```{r }
# WARNING: Sulcus order must match the following exactly:
# Left Lateral Fissure, Right Lateral Fissure,
# Left Anterior Cingulate Sulcus, Right Anterior Cingulate Sulcus,
# Left Posterior Cingulate Sulcus, Right Posterior Cingulate Sulcus,
# Left Calcarine Fissure, Right Calcarine Fissure,
# Left Collateral Fissure, Right Collateral Fissure,
# Left Intra-Parietal Fissure, Right Intra-Parietal Fissure,
# Left Parieto-Occipital Fissure, Left Occipital Sulcus,
# Right Occipital Sulcus, Left Central Sulcus, Right Central Sulcus,
# Left Inferior Frontal Sulcus, Right Inferior Frontal Sulcus,
# Left Paracingulate Sulcus, Right Paracingulate Sulcus,
# Left Intermediate Frontal Sulcus, Right Intermediate Frontal Sulcus,
# Left Superior Frontal Sulcus, Right Superior Frontal Sulcus,
# Left Occipital-Temporal Lateral Sulcus, Right Occipital-Temporal Lateral Sulcus,
# Left Orbitofrontal Sulcus, Right Orbitofrontal Sulcus,
# Left Medial Parietal Sulcus, Right Medial Parietal Sulcus,
# Left Pre-Central Sulcus, Right Pre-Central Sulcus,
# Left Post-Central Sulcus, Right Post-Central Sulcus,
# Left Inferior Temporal Sulcus, Right Inferior Temporal Sulcus,
# Left Superior Temporal Sulcus, Right Superior Temporal Sulcus,
# Right Parieto-Occipital Fissure
```


```{r }
library(ggplot2)
library(grid)
library(magrittr)
library(viridis)
library(raster)
library(readxl)
library(cowplot)
library(scales)
library(paletteer)
library(foreach)
library(doParallel)
if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}
library(magick)
```

```{r Helper functions}
# Recolors a sulcus image with a given color, removing background
recolor_sulcus <- function(sulcus_image, color, fuzz = 0) {
  image_transparent(image_background(image_transparent(sulcus_image, 'black', fuzz = fuzz), color), "white", fuzz = fuzz)
}

# Finds the closest index of a value in a reference vector
find_closest_index <- function(value, vector2) {
  which.min(abs(value - vector2))
}
```


```{r Main Function to Combine and Color Sulci}

combine_and_color_sulci_parallel_snyder_2024 <- function(sulci_values, color_palette, scale_width, bounds = NA, border_width = 0){
  base_image_path <- "C:/Users/Usuario/Desktop/Practicas/sulci_images_snyder_2024/"
  all_bg <- image_read(file.path(base_image_path, "all_brains.png"))
  
  new_names <- c("Lateral Fissure", "Anterior Cingulate Sulcus", "Posterior Cingulate Sulcus",
                   "Calcarine Fissure", "Collateral Fissure", "Intra-Parietal Fissure",
                   "Parieto-Occipital Fissure", "Occipital Sulcus", "Central Sulcus",
                   "Inferior Frontal Sulcus", "Paracingulate Sulcus", "Intermediate Frontal Sulcus",
                   "Superior Frontal Sulcus", "Occipital-Temporal Lateral Sulcus",
                   "Orbitofrontal Sulcus", "Medial Parietal Sulcus", "Pre-Central Sulcus",
                   "Post-Central Sulcus", "Inferior Temporal Sulcus", "Superior Temporal Sulcus")
  
  new_names_2 <- paste(rep(c("Left", "Right"), 20),rep(new_names, each = 2))
  new_names_2 <- new_names_2[-c(14)]
  new_names_2[40] <- "Right Parieto-Occipital Fissure"
  nomenclature <- new_names_2 # Assume this function generates your nomenclature
  remapped_sulci_values <- round(scales::rescale(sulci_values, to = c(1,1000)))
  if(!is.na(sum(bounds))){
    bounded_seq <- seq(from = bounds[1], to = bounds[2], length.out = 1000)
    remapped_sulci_values <- vapply(
    sulci_values,
    function(x) if (is.na(x)) NA_integer_ else find_closest_index(x, bounded_seq),
    integer(1)
    )
  }
  
  if(color_palette == "magma"){
    hex_codes <- magma(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if(color_palette == "bwr"){
    hex_codes <- colorRampPalette(c("blue","white", "red"))(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if(color_palette == "viridis"){
    hex_codes <- viridis(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if(color_palette == "plasma"){
    hex_codes <- plasma(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if(color_palette == "mako"){
    hex_codes <- mako(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if(color_palette == "cbp"){
    hex_codes <- colorRampPalette(c("cyan","blue", "purple"))(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if(color_palette == "ppy"){
    hex_codes <- colorRampPalette(c("purple","deeppink", "yellow"))(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if(color_palette == "white_blue"){
    hex_codes <- colorRampPalette(c("white", "darkblue"))(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if (color_palette == "broc") {
   hex_codes <- rev(paletteer_c("grDevices::Cork", 1000))
   sulci_hex_codes <- hex_codes[remapped_sulci_values]
   sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  if(color_palette == "gdb"){
    hex_codes <- colorRampPalette(c("darkgreen","darkblue"))(1000)
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
  }
  if (color_palette == "greys") {
    hex_codes <- paletteer_d("RColorBrewer::Greys")
    sulci_hex_codes <- hex_codes[remapped_sulci_values]
    sulci_hex_codes[is.na(sulci_hex_codes)] <- "#CCCCCC"
  }
  
  no_cores <- detectCores() - 1
  registerDoParallel(cores=no_cores)
  tmp_imgs <- foreach(i = 1:length(nomenclature), .packages = "magick", .export = c("recolor_sulcus", "find_closest_index")) %dopar% {
    file_path <- paste(base_image_path, nomenclature[i], ".png", sep = "")
    tmp_img <- image_read(file_path)
    tmp_img <- recolor_sulcus(tmp_img, sulci_hex_codes[i])
    if(border_width > 0){
      border_img <- image_morphology(image = tmp_img, operation = "dilate", kernel = "Disk", radius = border_width)
      tmp_img <- image_composite(border_img, tmp_img)
    }
    # Write to a temporary file and return the file path
    image_write(tmp_img, format = "png")  
  }
  
  # Composite all images onto the base image
  composite_img <- all_bg
  for (img_path in tmp_imgs) {
    tmp_img <- image_read(img_path)
    composite_img <- image_composite(composite_img, tmp_img)
  }
  
  # Stop the parallel backend
  stopImplicitCluster()
  
  return(image_scale(image_transparent(composite_img, "white"), scale_width))
}

```

```{r Load input data}
t_test_data <- read.csv("C:/Users/Usuario/Desktop/Practicas/AAA_80clinical/Train_Test/t-test/all_tests_width.csv")
sulci_values <- t_test_data$t_value
min_val <- min(sulci_values, na.rm = TRUE)
max_val <- max(sulci_values, na.rm = TRUE)
```

```{r }
sulcal_map <- combine_and_color_sulci_parallel_snyder_2024(
  sulci_values,
  color_palette = "white_blue",
  scale_width = "9000x",
  border_width = 0,
  bounds = c(min_val, max_val)
)
```

```{r Plot sulcal map}
ggplot() +
  annotation_custom(rasterGrob(sulcal_map)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  ) -> sulcal_plot

print(sulcal_plot)
```

```{r Save sulcal map}
ggsave(
  filename = "C:/Users/Usuario/Desktop/sulcal_map.png",
  plot = sulcal_plot,
  width = 12, height = 3, dpi = 300, bg = "transparent"
)
```

```{r Generate horizontal colorbar}
library(fields)
png(filename = "C:/Users/Usuario/Desktop/sulcal_map_colorbar.png", width = 1000, height = 200, bg = "transparent")
par(mar = c(1, 1, 3, 1))
image.plot(
  zlim = c(min_val, max_val),
  col = colorRampPalette(c("white", "darkblue"))(100),
  legend.only = TRUE,
  horizontal = TRUE,
  smallplot = c(0.1, 0.9, 0.35, 0.5),
  axis.args = list(cex.axis = 1.5),
  legend.args = list(text = expression(omega), side = 3, line = 1, cex = 1.8)
)
dev.off()
```
