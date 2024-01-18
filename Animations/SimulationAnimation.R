# Title: Animation for simulating distribution-based variables
# Author: Gareth Burns
# LinkedIn: https://www.linkedin.com/in/drgarethburns/
# GitHub: https://github.com/GABurns
# Twitter: @GarethBurns4
# Description:
#   The aim of this script was to create an animation that could be used to
#   show students at the University of Lancaster how we simulate variables
#   using a distribution at Exploristics in KerusCloud - our cloud-based
#   clincial trial simulation platform.
#   The effect I wanted to create was generating individuals seems random at first
#   but as you add individuals they follow a distribution. To create this effect
#   it's effectively two overlapping plots with a shared y-axis. The first plot
#   is a standard distribution density plot and the second is just strategically
#   placed images to create a dotplot with dots replaced with icons (of individuals)
#   I used ImageMagick to compile it into a .gif as I'd initially planed to add some
#   additional details outside of the plots but never did - I'm sure this script
#   could be modified to use gganimate.
# Link: https://www.linkedin.com/posts/drgarethburns_simulation-rstats-rshiny-activity-7128674509837987841-GT5i?utm_source=share&utm_medium=member_desktop
# External Dependencies: ImageMagick
# Creation Date: 13/11/2023
# Version Data: 13/11/2023
# Change log
# - Initial script

#   I had to do a couple of hacks to get this plot to work they way I wanted
#   but would I love about the open-source community is people will build and
#   improve upon it!
#   Really excited to see what you all do and be sure to share with me
#   Gareth Burns!


# Load Libraries ----------------------------------------------------------

library(ggplot2)
library(emojifont)
library(dplyr)
library(tidyr)

# Options -----------------------------------------------------------------
# Set the seed if you want to recreate as your generating random values
# set.seed(56161)


# User-created variables --------------------------------------------------

# I choose a bi-modal distribution using the example of height between sexes
# whereby I kept the standard deviation low to make 2 distinct but overlapping
# populations (so not real data, made up to be aesthically pleasing).
# Look up rnorm ?rnorm if you're not familiar with generating random data in R.
data <- data.frame(Gender = c(rep("Male", 250), rep("Female", 250)),
                   Height = c(rnorm(250, 165, 5), rnorm(250, 150, 4)))



# Plot --------------------------------------------------------------------

# The intial base plot I wanted was the observed densities as two distinct lines
# this was created below
# Note: If you leave out the colour argument is creates a density distribution as
# a single line with a cumlative distribution - useful if you're talking about
# bi-modal data but don't know/have the factor was makes it bi-modal - we find this
# with biomarkers.

plot <- ggplot(data, aes(x = Height, colour = Gender)) +
  geom_density() +
  labs(x = "Height (cm)", y = "Frequency") +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 16, hjust  = 0.1))


# At this stage I experimented with a lot to get the effect of individuals being
# simulated. I thought a modified dotplot whereby each dot represents a count
# within a bin. I couldn't find a way to modify the dotplot to use an image\icon
# so settled a hacky approach....using geom_fontawesome from the emojifont package


# Data Wrangling ----------------------------------------------------------

# geom_fontawesome takes an x and a y so now I need to do some data wrangling to
# achieve this values. The x is the hieght interval - i.e. I want everyone with a
# 160 cm height into the same bin. In ecology this is often known as length/height
# frequency.

# This approach was very iterative where I went back and forward to see looked
# aesthetically pleasing. In my plot each icon represents 5 individuals within a
# 2cm bin - i.e. in my plot the icon doesn't represent an individual you could.

iconData <- data %>%
  group_by(Gender,
           Interval = cut_width(Height, width = 2, boundary = min(Height))) %>%
  summarise(n = n()) %>%
  mutate(Height = substr(Interval, 2, 4)) # Presentation label for plot

# The next step was to get the number individuals I wanted my icons to
# represent in the plot. If you want each icon to represent an individual these
# 2 steps aren't needed. Just round your data to what you see fit

iconData <-  iconData %>%
  mutate(iconCount = ceiling(n / 5)) # The ceiling was the effect I wanted
# Could use floor or consider the size of your icon

# Uncounting
# A common issue in ecology is we collect frequency tallies but when using the
# data we want an indivudal row per sample/individual - so it's inverse of count
# function in dplyr. For the y - we want the position on the y axis and
# this is quite subjective as it'll depend on the size of your plot, the max value
# of the frequency density and the size of your icons.
# As a rule of thumb you want the max density / max number of icons in a bin

iconData <- iconData %>%
  uncount(iconCount) %>%
  group_by(Gender, Height) %>%
  mutate(y = row_number() * 0.008) # Modify this value to change spacing between
# icons.

# Plotting Icons ----------------------------------------------------------

plot <- ggplot(data, aes(x = Height, colour = Gender)) +
  geom_density(show.legend = FALSE) +
  labs(x = "Height (cm)", y = "Frequency") +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 16, hjust  = 0.1)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.12)) + # Fix y lims as graph resizes and messes
  geom_fontawesome(
    alias = "fa-child",
    x = as.numeric(iconData$Height),
    y = as.numeric(iconData$y),
    size = 7, # Modify the size of
    color = ifelse(iconData$Gender == "Male", "dodgerblue3", "red")
  )

# View the plot
plot

# My workflow was to use the ImageMagick as it would provide flexabiltity to
# add frames outside of the plot animation (although I never did).
# Think of how an animations works - it's a series of static images displayed
# in a sequential order. What we're doing here is creating the individual
# plots (images) that add icons sequentially.

# My first thought was randomly sampling all the rows...however the issue was that
# this had a y value and would create floating icons. To overcome this I sampled
# from the iconData dataset to sequentially build the dataset to plot but recreate
# the y-axis.
# Note: This iterative approach to building a dataset is computational intense so
# won't scale very well.

sampleIndex <- sample(nrow(iconData), replace = FALSE)


for (i in seq_len(length(sampleIndex))) {

  iconDataLoop <- iconData[sampleIndex[1:i],] %>%
    group_by(Gender, Height) %>%
    mutate(y = row_number() * 0.008)

  plot <- ggplot(data, aes(x = Height, colour = Gender)) +
    geom_density(show.legend = FALSE) +
    labs(x = "Height (cm)", y = "Frequency") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 16, hjust  = 0.1),
          panel.grid.major =  element_blank(),
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, 0.12)) + # Fix y lims as graph resizes and messes
    geom_fontawesome(
      alias = "fa-child",
      x = as.numeric(iconDataLoop$Height),
      y = as.numeric(iconDataLoop$y),
      size = 6, # Modify the size of icon depenents of resolution of plot,
      # a higher resolution will need a larger size argument.
      color = ifelse(iconDataLoop$Gender == "Male", "dodgerblue3", "red")
    )

  # Dependent on the size of your saved image effects the size
  # of the icons.
  ggsave(
    filename = paste0("frames/plot", sprintf("%03d", i) , ".jpg"),
    plot = plot,
    device = "jpeg",
    height = 500,
    width = 919,
    units = "px"
  )
}


# Animation ---------------------------------------------------------------

# The blog https://www.r-bloggers.com/2018/05/animating-a-monte-carlo-simulation/
# describes this process. Install ImageMagick and run
# 'convert -delay 20 -loop 0 *.jpg ../animation.gif' in terminal

# Or for an workflow only in R you could use ggnimate and transistional_revel
# for each icon. ggnimate is really powerful so I'd recommend playing about
# with it and posting your solution in comments for others to use!
