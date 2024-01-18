# Title:  Radar Plot
# Author: Gareth Burns
# Creation Date: 18/01/2024
# Description: Radar Plot in Response to "What type of Problem Solver are you?"
# blog of Kim Hacquoil


# Load Libraries ----------------------------------------------------------

# devtools::install_github("topfunky/gghighcontrast")

library(ggplot2)
library(ggradar)
library(gghighcontrast)
library(ggtext)
library(emojifont)


# User Defined Variables --------------------------------------------------

github_icon <- "&#xf09b"
github_username <- "GABurns"


social_caption <- sprintf(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{%s};</span>
  <span style='color: #E30B5C'>{%s}</span>",
  github_icon,
  github_username
)



# Create Data -------------------------------------------------------------


data  <- data.frame(Group = "Gareth",
                    Analytical = 90,
                    Creator = 60,
                    Collaborator = 70,
                    Connector = 50,
                    Pragmatist = 40)



# Plot --------------------------------------------------------------------


plot <- ggradar(
  data,
  values.radar = c("0", "50", "100"),
  font.radar = "mono",
  grid.min = 0, grid.mid = 50, grid.max = 100,
  # Polygons
  group.line.width = 1,
  group.point.size = 3,
  group.colours = "green",
  # Background and grid lines
  background.circle.colour = "green",
  gridline.mid.colour = "green",
  gridline.min.colour = "green",
  gridline.max.colour = "green",
  axis.line.colour = "green",
) +
  ggtitle(label = "What type of problem solver am I?",
          subtitle ="Probably analytical!") +
  theme_high_contrast(
    foreground_color = "white",
    background_color = "black",
    base_family = "InputMono"
  ) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        title = element_text(colour = "green", family = "mono"),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_textbox_simple()
  )


plot$layers[[1]]$aes_params <- c(plot$layers[[1]]$aes_params, colour = "green")
plot$layers[[1]]$aes_params$size <- 3.5
plot$layers[[5]]$aes_params <- c(plot$layers[[5]]$aes_params, colour = "green")
plot$layers[[5]]$aes_params$size <- 3.5
plot$layers[[6]]$aes_params <- c(plot$layers[[6]]$aes_params, colour = "green")
plot$layers[[6]]$aes_params$size <- 3.5
plot$layers[[11]]$aes_params <- c(plot$layers[[11]]$aes_params, colour = "green")
plot$layers[[12]]$aes_params <- c(plot$layers[[12]]$aes_params, colour = "green")
plot$layers[[13]]$aes_params <- c(plot$layers[[13]]$aes_params, colour = "green")

plot
