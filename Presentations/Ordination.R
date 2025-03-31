# The R code which can be used to reproduce the ordination
library (vegan) # uploading vegan library
library(ggvegan)
library(tidyverse)
library(ggordiplots)
# Import of data from GitHub repository:
spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/grasslands-spe.txt', row.names = 1)
env0 <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/grasslands-env.txt', row.names = 1)
env <- env0[,c('altitude', 'slope', 'pH.H', 'soildepth',"cover_moss")]
g <- as.numeric (env0$classification)


# Calculating ordination:
#spe.dist <- vegdist (spe, method = 'bray') # Calculating distance matrix
ordi <- decorana (spe)

ordi_table <-  fortify( ordi ) |> 
  #filter(score == "sites") |> 
  mutate(DCA1=as.numeric(DCA1),
         DCA2=as.numeric(DCA2))
ef <- envfit (ordi, env)
vector <- fortify(ef)|> as_tibble()

rslt.hull <- vegan::ordihull(ordi, groups = env0$classification, scaling = scaling, draw = "none")
df_hull <- data.frame()
df_temp <- data.frame()
groups<-as.factor(env0$classification)
show.groups <- as.vector(levels(groups))
for (g in show.groups) {
  x <- rslt.hull[[g]][, 1]
  y <- rslt.hull[[g]][, 2]
  Group <- rep(g, length(x))
  df_temp <- data.frame(Group = Group, x = x, y = y)
  df_hull <- rbind(df_hull, df_temp)
}



ordi_table|> 
  ggplot(aes(x = DCA1  ,
             y = DCA2)) +
  geom_polygon(data = df_hull, aes(x = x, y = y, 
                                   fill = Group), show.legend = TRUE,alpha=0.4)+
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_hline(yintercept = 0, colour = "grey60") +
  coord_equal() +
  geom_point(aes(shape=score),size = 1, alpha = 0.5)+
  scale_shape_manual("", labels = c("sites","species"),  values = c(16,3)) +
  geom_segment(
    data = vector,
    aes(
      x = 0,
      xend = DCA1*4,
      y = 0,
      yend = DCA2*4
    ),
    arrow = arrow(length = unit(0.25, "cm")),
    colour = "darkred",
    lwd = 0.8
  ) +
  ggrepel::geom_text_repel(
    data =  vector,
    aes(x = DCA1*4.2,
        y = DCA2*4.2, 
        label = c("Elevation","Slope","pH","Soildepth","Cover moss")
    ),
    direction = "both",
    segment.size = 0.07,
    size = 4, #font size
    colour = "black"
  )+
  labs(
    x = paste0("DCA1 (",(round(ordi$evals[1] , 1))*100,"%)"),
    y = paste0("DCA2 (",(round(ordi$evals[2] , 1))*100,"%)")
  ) +
  theme_bw()



# Ploting ordination diagram:
ordiplot (ordi, type = 'n')
points (ordi, display = 'species', pch = '+', col = 'red')
points (ordi, display = 'sites', pch = as.character (groups), col = groups)
for (gr in unique (groups)) ordihull (ordi, groups = groups, show.group = gr, draw = 'polygon', col = gr, border = gr, alpha = 50)

plot (ef)
