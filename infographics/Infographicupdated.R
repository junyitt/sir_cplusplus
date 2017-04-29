# Simulate data from normal distribution
y1 <- round(rnorm(n = 36, mean = 7, sd = 2))
y2 <- round(rnorm(n = 36, mean = 21, sd = 6))
y3 <- round(rnorm(n = 36, mean = 50, sd = 8))
x <- rep(LETTERS[1:12], 3)
grp <- rep(c("Grp 1", "Grp 2", "Grp 3"), each = 12)
dat <- data.frame(grp, x, y1, y2, y3)

#font 
windowsFonts(
  B=windowsFont("Bookman Old Style")
  )	

# Configure Theme
kobe_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "B"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "B"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "B"),
    panel.grid.major.x = element_line(colour = "#E7A922"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "B", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}

# Generate Infographic in PDF format
library(grid)
pdf("C:/Users/sheey/Desktop/sir/myfile/Infographicstes7.pdf", width = 15, height = 20)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 3)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("INFOGRAPHIC", y = unit(0.98, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(family = "B", col = "#A9A8A7", cex = 12, alpha = 0.3))
grid.text("Pandemic Outbreak", y = unit(0.94, "npc"), gp = gpar(family = "B", col = "#E7A922", cex = 6.4))
grid.text("BY Team Miracle", vjust = 0, y = unit(0.91, "npc"), gp = gpar(family = "B", col = "#552683", cex = 0.8))
grid.text("NASA Apps Challenge 2017", vjust = 0, y = unit(0.903, "npc"), gp = gpar(family = "B", col = "#552683", cex = 0.8))


grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("CATEGORY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(family = "B", col = "#CA8B01", cex = 13, alpha = 0.3))
grid.text("PANDEMIC OUTBREAK SIMULATION", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(family = "B", col = "#552683", cex = 1.2))
grid.text("DATA INFO", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(family = "B", col = "white", cex = 1.2))
grid.text(paste(
  "Syndicated to",
  "Author",
  "Maintainer",
  "Granularity",
  "Temporal Date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.79, "npc"), gp = gpar(family = "B", col = "#552683", cex = 0.8))
grid.text(paste(
  "https://jycheah.shinyapps.io/beta2",
  "Analysis with Programming",
  "Team Miracle",
  "National",
  "2017", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.79, "npc"), gp = gpar(family = "B", col = "#552683", cex = 0.8))

#pie chart y3
#p3 <- pie(data = dat, aes(x = reorder(x, rep(1:12, 3)), y = y3, group = factor(grp))) +
#  geom_bar(stat = "identity", fill = "#552683") + coord_polar() + facet_grid(. ~ grp) +
#  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("SIR MODEL")
p3 <- pie(y3, main = "SIR MODEL", col = "#552683")
print(p3, vp = vplayout(4, 1:3))

#line graph y2
#p2 <- plot(data = dat, aes(x = x, y = y2, group = factor(grp))) +
#  geom_line(stat = "identity", aes(linetype = factor(grp)), size = 0.7, colour = "#552683") +
#  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE")
#p2 <- lines(y2, main = "TITLE", col = "#552683", xlab = "X LABEL", ylab = "Y LABEL")
#print(p2, vp = vplayout(2, 1:3))

#bar chart y1
#x_id <- rep(12:1, 3) # use this index for reordering the x ticks
#p1 <- barplot(data = dat, aes(x = reorder(x, x_id), y = y1)) + geom_bar(stat = "identity", fill = "#552683") +
#  coord_flip() + ylab("Y LABEL") + xlab("X LABEL") + facet_grid(. ~ grp) +
#  ggtitle("TITLE OF THE FIGURE")
#p1 + kobe_theme()
p1 <- barplot(y1, main = "TITLE", col = "#552683", xlab = "X LABEL", ylab = "Y LABEL") 
#print(p1, vp = vplayout(3, 1:3))

dev.off()
