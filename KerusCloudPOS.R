# Title:  KerusCloud POS
# Author: Gareth Burns
# Creation Date: 09/01/2024
# Description: Moving barchart animation of the POS of KerusCloud POS compared to
# Links: https://r-graph-gallery.com/288-animated-barplot-transition.html
# https://r-graph-gallery.com/304-highlight-a-group-in-lollipop.html

# Load Libraries ----------------------------------------------------------

library(tidytuesdayR)


# Create Data -------------------------------------------------------------

data <-  data.frame(Company = c("KerusCloud", "AreaA", "AreaB", "AreaC", "AreaD"),
                    POS = c(75, 10, 15, 20, 25))


ggplot(data, aes(x=Company, y=POS)) +
  geom_segment(
    aes(x=Company, xend=Company, y=0, yend=POS,
        color=ifelse(data$Company %in% c("KerusCloud"), "orange", "grey"),
        size=ifelse(data$Company %in% c("KerusCloud"), 1.3, 0.7))
  ) +
  geom_point(
    color= ifelse(data$Company %in% c("KerusCloud"), "orange", "grey"),
    size= ifelse(data$Company %in% c("KerusCloud"), 5, 2)
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  xlab("") +
  ylab("Percent of trials that succeed") +
  ggtitle("KerusCloud powered trials improve your probability of success") +
  theme(plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
