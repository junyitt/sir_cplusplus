# # Simulate data from normal distribution
# y1 <- round(rnorm(n = 36, mean = 7, sd = 2))
# y2 <- round(rnorm(n = 36, mean = 21, sd = 6))
# y3 <- round(rnorm(n = 36, mean = 50, sd = 8))
# x <- rep(LETTERS[1:12], 3)
# grp <- rep(c("Grp 1", "Grp 2", "Grp 3"), each = 12)
# dat <- data.frame(grp, x, y1, y2, y3)

urlf <- "https://raw.githubusercontent.com/junyitt/sir_cplusplus/master/data/output_sir.csv"

library(curl)
fdf <- read.csv( curl(urlf) )

### graph
      df <- fdf[1:5]
      df2 <- melt(df, id.vars = c("t", "place")); df2[,"place"] <- factor(df2[,"place"], levels=c(1,2,3), labels = placeName, ordered=TRUE)
      # ggplot(df2, aes(x=t, y = value, group = variable, color = variable)) + geom_point()
      
      # ggplot(df2, aes(x=t, y = value, colour=variable)) + geom_point() + facet_grid(place~.)
      L1<-lapply(unique(df2[,"place"]), FUN = function(x){
            u1 <- df2[,"place"] == x
            df3 <- df2[u1,]
            g <- ggplot(df3, aes(x=t, y = value, colour=variable)) + geom_point() + ggtitle(x)
            return(g)
      })


###chart
      t0 <- lapply(0.05, FUN = function(x){
            u1 <- fdf[,"t"] == x
            fdf[u1,][2:4]
      })[[1]]
            
      list.place.df <- lapply(unique(fdf[,"place"]), FUN = function(x){ u1 <- fdf[,"place"] == x; fdf[u1,] }) 
      
      tpeak <- lapply(list.place.df, FUN = function(dff){
            u1 <- which(dff[,"i"] == max(dff[,"i"]))
            dff[u1,][2:4]
      })
      tpeak <- rbind.fill(tpeak)

# pie1:
setwd("C:/Users/User/git/sir_cplusplus/infographics")
png("pie1.png")
par(mfrow=(c(1,2)))
slices <- as.numeric(t0[1,])
lbls <- paste0(c("S", "I", "R"), ": ")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("yellow", "red", "green"),
    main="Proportion of SIR in MYS, t = 0")


slices <- as.numeric(tpeak[1,])
lbls <- paste0(c("S", "I", "R"), ": ")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("yellow", "red", "green"),
    main="when \"I\" peaked")

dev.off();



# pie2:
png("pie2.png")
par(mfrow=(c(1,2))); 
slices <- as.numeric(t0[2,])
lbls <- paste0(c("S", "I", "R"), ": ")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("yellow", "red", "green"),
    main="Proportion of SIR in SGP, t = 0")


slices <- as.numeric(tpeak[2,])
lbls <- paste0(c("S", "I", "R"), ": ")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("yellow", "red", "green"),
    main="When \"I\" peaked")
dev.off()


# pie3:
png("pie3.png")
par(mfrow=(c(1,2)))
slices <- as.numeric(t0[3,])
lbls <- paste0(c("S", "I", "R"), ": ")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("yellow", "red", "green"),
    main="Proportion of SIR in THA, t = 0")


slices <- as.numeric(tpeak[3,])
lbls <- paste0(c("S", "I", "R"), ": ")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("yellow", "red", "green"),
    main="when \"I\" peaked")

dev.off()

png("graph1.png")
      L1[[1]]
dev.off()

png("graph2.png")
L1[[2]]
dev.off()

png("graph3.png")
L1[[3]]
dev.off()

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
pdf("C:/Users/User/Desktop/InfoGraphics.pdf", width = 15, height = 20)

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
p3 <- pie(c(1,2,3), main = "SIR MODEL", col = "#552683", angle = c(45,45,180))
print(p3, vp = vplayout(4, 1:3))

slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")

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
# p1 <- barplot(y1, main = "TITLE", col = "#552683", xlab = "X LABEL", ylab = "Y LABEL") 
#print(p1, vp = vplayout(3, 1:3))

dev.off()
