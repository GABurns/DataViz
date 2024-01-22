# Title:  KerusCloud POS
# Author: Gareth Burns
# Creation Date: 09/01/2024
# Description: Moving barchart animation of the POS of KerusCloud POS compared to
# Links: https://r-graph-gallery.com/288-animated-barplot-transition.html
# https://r-graph-gallery.com/304-highlight-a-group-in-lollipop.html

# -------------------------
# NOTE NEED TO GET EXACT FIGURES!
# THESE ARE ALL EYE-BALLED FROM REPORT FIGURE
# -------------------------

# Load Libraries ----------------------------------------------------------
library(ggplot2)


# Create Data -------------------------------------------------------------

data <-
  data.frame(
    Company = c("KerusCloud", "Hematology", "Metabolic", "Infectious Disease", "Others", "Ophthalmology", "Autoimmune", "Allergy", "Gastroenterology", "Respiratory", "Psychiatry", "Endocrone", "Neurology", "Oncology", "Cardiovascular", "Urology"),
    POS = c(75, 24, 18, seq(13, 3, length.out = 13)  )
  )

data$POS_Label <- ifelse(data$Company %in% c("KerusCloud"), data$POS, "")

totalFrames <- 40

animationData <- lapply(seq_len(totalFrames), function(frame, data, total_frames) {
  frameIncrease <-  max(data$POS)/ total_frames
  frameValue <- frameIncrease * frame
  data$frame <-  frame
  data$transPOS <- ifelse(data$POS > frameValue, frameValue, data$POS)

  return(data)
}, data = data, total_frames = totalFrames)

animationData <- do.call("rbind", args = animationData)


ggplot(data, aes(x = Company, y = POS)) +
  geom_segment(aes(
    x = Company,
    xend = Company,
    y = 0,
    yend = POS,
  ),
  color = ifelse(data$Company %in% c("KerusCloud"), "#0084B0", "grey"),
  linewidth = ifelse(data$Company %in% c("KerusCloud"), 1.8, 1)) +
  geom_segment(aes(
    x = Company,
    xend = Company,
    y = 0,
    yend = POS,
  ),
  color = ifelse(data$Company %in% c("KerusCloud"), "#0084B0", "grey"),
  linewidth = ifelse(data$Company %in% c("KerusCloud"), 1.3, 0.7),
  alpha = 0.1) +
  geom_point(
    colour = ifelse(data$Company %in% c("KerusCloud"), "#0084B0", "grey"),
    size = ifelse(data$Company %in% c("KerusCloud"), 9, 2)
  ) +
  geom_point(
    colour = ifelse(data$Company %in% c("KerusCloud"), "#0084B0", "grey"),
    size = ifelse(data$Company %in% c("KerusCloud"), 10, 3),
    alpha = 0.1
  ) +
  geom_text(aes(label = POS_Label), size = 4, colour =  ifelse(data$Company %in% c("KerusCloud"), "#00354E", "grey")) +
  geom_text(aes(label = POS_Label), size = 4.4, alpha = 0.5,  colour =  ifelse(data$Company %in% c("KerusCloud"), "#00354E", "grey")) +
  scale_x_discrete(limits = data$Company[order(data$Company, decreasing = TRUE)])+ #[order(data$POS, decreasing = FALSE)]) + # FEEDBACK for unordered
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Successful trials (%)") +
  ggtitle("KerusCloud improves your clinicial trial success",
          subtitle = "Trials that use KerusCloud to optimise study design have a 3x greater success rate") +
  theme(
    plot.title = element_text(size = 18, colour = "#00354E"),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "#0084B0"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(colour = "#00354E"),
    axis.text= element_text(colour = "#0084B0")
  ) +
  labs(caption = "Source: Clinical Development Success Rates and Contributing Factors 2011-2020")


# Animation

ggplot(animationData, aes(x = Company, y = transPOS)) +
  geom_segment(aes(
    x = Company,
    xend = Company,
    y = 0,
    yend = transPOS,
  ),
  color = ifelse(animationData$Company %in% c("KerusCloud"), "0084B0", "grey"),
  linewidth = ifelse(animationData$Company %in% c("KerusCloud"), 1.3, 0.7)) +
  geom_point(
    colour = ifelse(animationData$Company %in% c("KerusCloud"), "0084B0", "grey"),
    size = ifelse(animationData$Company %in% c("KerusCloud"), 4, 2)
  ) +
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Successfull trials (%)") +
  ggtitle("KerusCloud improves your clinicial trial success",
          subtitle = "Trials that use KerusCloud have a 3x greater success rate") +
  theme(
    plot.title = element_text(size = 18),
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(caption = "Source: Clinical Development Success Rates and Contributing Factors 2011-2020") +
  # gganimate specific bits:
  transition_states(
    frame,
    transition_length = 2,
    state_length = 0.01
  ) +
  ease_aes("sine-in-out")
