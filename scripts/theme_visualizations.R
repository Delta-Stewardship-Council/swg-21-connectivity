theme_vis <- theme_bw() +
  theme(strip.background = element_rect(color = "black", fill = "white"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

library(ggplot2)
str(iris)

ggplot(iris) +
  geom_point(aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  facet_wrap(~Species) +
  theme_vis

# Factor order:
## none, short, long
## upstream, yolo, downstream
## viridis color palette?

