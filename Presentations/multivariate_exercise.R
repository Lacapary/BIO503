load("input_08_itex_all.RData")

itex.ordination <- itex.all %>% 
  filter(SITE %in% c("BARROW", "ATQASUK"))

ordination <- itex.ordination %>% 
  filter(SUBSITE %in% c("BD", "BW", "AD", "AW")) %>% 
  filter(!grepl("XXX", SPECIES_NAME)) %>% 
  filter(STATUS == "LIVE") %>% 
  filter(TREATMENT %in% c("CONTROL", "CTL")) %>% 
  group_by(SITE, SUBSITE, PLOT) %>% 
  mutate(remove = case_when(
    YEAR == max(YEAR) ~ "NO", TRUE  ~ "YES")) %>% 
  filter(remove == "NO")


itex.long <- ordination %>% 
  dplyr::select(SITE, SUBSITE, PLOT, SPECIES_NAME, SumAbund)

itex.wide <- itex.long %>% 
  pivot_wider(names_from  = SPECIES_NAME, values_from  = SumAbund, values_fill = 0) 

nmds1 <- itex.nmds %>% 
  ungroup() %>% 
  dplyr::select(4:101)

dist <- vegdist(nmds1,  method = "bray")

NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

NMDS.scree(dist)


set.seed(2)

# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
# Do you know what the trymax = 100 and trace = F means?
# Let's check the results
NMDS1

# If you don`t provide a dissimilarity matrix, metaMDS automatically applies Bray-Curtis. So in our case, the results would have to be the same
NMDS2 <- metaMDS(nmds1, k = 2, trymax = 100, trace = F)
NMDS2

stressplot(NMDS1)

plot(NMDS1, type = "t")


plot(NMDS1,)

NMDS3 <- metaMDS(nmds1, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

# If we want to group the sites
group = c(rep("BD", 24), rep("BW", 24), rep("AD", 24), rep("AW", 24))
colors = c(rep("#dda15e", 24), rep("#7A3100", 24), rep("#606c38", 24), rep("#283618", 24))
moisture = c(rep("DRY", 24), rep("WET", 24), rep("DRY", 24), rep("WET", 24))
temperature = c(rep("", 24), rep("WET", 24), rep("DRY", 24), rep("WET", 24))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")
for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3, display = "species", col = "#444444", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("#dda15e",24),
                                           rep("#7A3100", 24),
                                           rep("#606c38", 24),
                                           rep("#283618", 24)), air = 0.01, cex = 1.25)
