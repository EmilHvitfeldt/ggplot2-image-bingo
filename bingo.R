library(tidyverse)
library(ggimage)

img <- list.files("images", pattern = "png", full.names = TRUE)
bingo <- tibble(
  x = 1:5,
  y = 6,
  label = c("B", "I", "N", "G", "O")
)

for (i in 1:30) {
  expand_grid(x = 1:5, y = 1:5) |>
    filter(!(x == 3 & y == 3)) |>
    slice_sample(prop = 1) |>
    mutate(image = img) |>
    ggplot(aes(x, y)) +
    geom_image(aes(image=image), size = 0.13, asp = 1) +
    geom_rect(aes(xmin = x - 0.5, xmax = x + 0.5,
                  ymin = y - 0.5, ymax = y + 0.5),
              fill = "#00000000", color = "black") +
    geom_text(aes(label = label), data = bingo, size = 20) +
    annotate("text", x = 3, y = 3, label = "FREE\nSPACE", size = 10) +
    scale_size_identity() +
    lims(x = c(0.5, 5.5), y = c(0.5, 6.5)) +
    coord_fixed() +
    theme_void()

  ggsave(filename = paste0("plates/", i, ".pdf"), device = "pdf", width = 8.5, height = 11, units = "in")
}
