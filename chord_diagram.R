library(circlize)
library(tidyr)
library(dplyr)

skillCat <- read.csv('data/skill_cat.csv')

pvt <- skillCat %>%
  pivot_wider(names_from = skill, values_from = number) %>%
  group_by(category) %>%
  summarise(across(c("correctness", "readability", "maintainability", "documentation"), ~sum(., na.rm = TRUE)))

pvt <- as.data.frame(pvt)
rownames(pvt) <- pvt$category
df <- pvt[c("correctness", "readability", "maintainability", "documentation")]

head(df)

grid.col <- c(correctness = "red", maintainability = "green", readability = "blue", documentation = "yellow",
              dynamic = "grey", static = "grey", ml = "grey")


pdf(file = "plots/skill_cat_chord.pdf", width = 10, height = 10)

circos.par(start.degree = 270, circle.margin = c(0.1, 0.1, 0.1, 0.1))
chordDiagram(t(df), directional = -1, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim <- get.cell.meta.data("xlim")
  ylim <- get.cell.meta.data("ylim")
  sector.name <- get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

dev.off()
circos.clear()