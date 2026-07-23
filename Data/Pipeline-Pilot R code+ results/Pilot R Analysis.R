#Installing and calling necessary packages
install.packages("tidyverse")
library(tidyverse)

#Reading the merged file 
data_raw <- read_csv(
  "/Users/taravat/Desktop/QP/QP/Data/Pipeline-Pilot Data/pse-sandbox-merged.csv"
)


#---------------------------------------------------------
# Figure 1: Mean certainty by predicate
#---------------------------------------------------------

#selecting only the variables needed for Figure 1
figure1_data <- data_raw %>%
  select(
    workerid,
    verb,
    response
  )

#Making sure our data is numeric 
figure1_data <- figure1_data %>%
  mutate(
    response = as.numeric(response)
  )

#Removing missing responses
figure1_data <- figure1_data %>%
  filter(
    !is.na(response),
    !is.na(verb),
    !is.na(workerid)
  )

#Changing the verb inform_Sam to inform 
figure1_data <- figure1_data %>%
  mutate(
    verb = recode(
      verb,
      "inform_Sam" = "inform"
    )
  )

#Categorizing based on the type of factivity to control the colors and shapes in the figures
figure1_data <- figure1_data %>%
  mutate(
    predicate_type = case_when(
      
      verb == "control" ~
        "Control",
      
      verb %in% c(
        "think",
        "suggest",
        "say"
      ) ~
        "Nonfactive",
      
      verb %in% c(
        "prove",
        "confirm",
        "establish",
        "acknowledge",
        "hear",
        "inform"
      ) ~
        "Optionally factive",
      
      verb %in% c(
        "discover",
        "know",
        "reveal"
      ) ~
        "Canonically factive",
      
      TRUE ~ NA_character_
    )
  )

#For average responses within participant by predicate
participant_predicate_data <- figure1_data %>%
  group_by(
    workerid,
    verb,
    predicate_type
  ) %>%
  summarise(
    participant_rating = mean(
      response,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

#Calculating the mean for each predicate
predicate_summary <- participant_predicate_data %>%
  group_by(
    verb,
    predicate_type
  ) %>%
  summarise(
    mean_certainty = mean(
      participant_rating,
      na.rm = TRUE
    ),
    number_of_participants = n(),
    .groups = "drop"
  )

#Creating a function to calculate a 95% bootstrapped confidence interval
bootstrap_mean_ci <- function(
    values,
    number_of_bootstraps = 5000
) {
  
  #Removing missing values
  values <- values[!is.na(values)]
  
  #Creating 5000 bootstrap means
  bootstrap_means <- replicate(
    number_of_bootstraps,
    mean(
      sample(
        values,
        size = length(values),
        replace = TRUE
      )
    )
  )
  
  #Returning the lower and upper limits of the confidence interval
  tibble(
    lower_ci = quantile(
      bootstrap_means,
      probs = 0.025
    ),
    
    upper_ci = quantile(
      bootstrap_means,
      probs = 0.975
    )
  )
}

#Making the bootstrap results reproducible
set.seed(2026)

#Calculating bootstrap confidence intervals for each predicate
predicate_bootstrap_ci <- participant_predicate_data %>%
  group_by(
    verb,
    predicate_type
  ) %>%
  group_modify(
    ~ bootstrap_mean_ci(
      .x$participant_rating,
      number_of_bootstraps = 5000
    )
  ) %>%
  ungroup()

#Combining predicate means and bootstrap confidence intervals
predicate_summary <- predicate_summary %>%
  left_join(
    predicate_bootstrap_ci,
    by = c(
      "verb",
      "predicate_type"
    )
  )

#Ordering predicates from lowest to highest mean certainty
predicate_order <- predicate_summary %>%
  arrange(mean_certainty) %>%
  pull(verb) %>%
  as.character()

#Applying the order to both plotting datasets
participant_predicate_data <- participant_predicate_data %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = predicate_order,
      ordered = TRUE
    )
  )

predicate_summary <- predicate_summary %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = predicate_order,
      ordered = TRUE
    )
  )

#Ordering predicates from lowest to highest mean certainty
predicate_order <- predicate_summary %>%
  arrange(
    mean_certainty
  ) %>%
  pull(
    verb
  ) %>%
  as.character()

#Applying the predicate order to the participant-level data
participant_predicate_data <- participant_predicate_data %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = predicate_order
    )
  )

#Applying the same predicate order to the summary data
predicate_summary <- predicate_summary %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = predicate_order
    )
  )

#Checking that the order is correct
levels(
  participant_predicate_data$verb
)

#Creating Figure 1
figure1 <- ggplot(
  participant_predicate_data,
  aes(
    x = verb,
    y = participant_rating
  )
) +
  
  #Showing the distribution of participant-level ratings
  geom_violin(
    aes(
      group = verb
    ),
    fill = "white",
    color = "grey80",
    linewidth = 0.6,
    width = 0.85,
    scale = "width",
    trim = FALSE
  ) +
  
  #Showing 95% bootstrapped confidence intervals
  geom_errorbar(
    data = predicate_summary,
    aes(
      x = verb,
      ymin = lower_ci,
      ymax = upper_ci
    ),
    inherit.aes = FALSE,
    color = "black",
    width = 0.10,
    linewidth = 0.7
  ) +
  
  #Showing predicate means
  geom_point(
    data = predicate_summary,
    aes(
      x = verb,
      y = mean_certainty,
      color = predicate_type,
      shape = predicate_type
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  
  #Matching colors to predicate categories
  scale_color_manual(
    values = c(
      "Control" = "black",
      "Nonfactive" = "grey55",
      "Optionally factive" = "#F15A3A",
      "Canonically factive" = "#9C39C6"
    ),
    breaks = c(
      "Control",
      "Nonfactive",
      "Optionally factive",
      "Canonically factive"
    ),
    labels = c(
      "Control" = "main clause controls",
      "Nonfactive" = "nonfactive",
      "Optionally factive" = "optionally factive",
      "Canonically factive" = "factive"
    )
  ) +
  
  #Matching shapes to predicate categories
  scale_shape_manual(
    values = c(
      "Control" = 16,
      "Nonfactive" = 15,
      "Optionally factive" = 17,
      "Canonically factive" = 18
    ),
    breaks = c(
      "Control",
      "Nonfactive",
      "Optionally factive",
      "Canonically factive"
    ),
    labels = c(
      "Control" = "main clause controls",
      "Nonfactive" = "nonfactive",
      "Optionally factive" = "optionally factive",
      "Canonically factive" = "factive"
    )
  ) +
  
  #Displaying control as MC on the x-axis
  scale_x_discrete(
    limits = predicate_order,
    labels = function(x) {
      ifelse(
        x == "control",
        "MC",
        x
      )
    }
  ) +
  
  #Showing ticks from 0 to 1
  scale_y_continuous(
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    expand = expansion(
      mult = c(0.01, 0.03)
    )
  ) +
  
  #Displaying only the response range without deleting violin coordinates
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  #Adding axis and legend labels
  labs(
    x = "Predicate",
    y = "Mean certainty rating",
    color = "Predicate type",
    shape = "Predicate type"
  ) +
  
  #Using a style closer to the published figure
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 50,
      hjust = 1,
      vjust = 1
    ),
    
    legend.position = "bottom",
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.6
    )
  ) +
  
  guides(
    color = guide_legend(
      title.position = "left",
      nrow = 1
    ),
    
    shape = guide_legend(
      title.position = "left",
      nrow = 1
    )
  )

#Displaying Figure 1
figure1


#---------------------------------------------------------
# Figure 2: Mean certainty by predicate and boundedness
#---------------------------------------------------------

#Selecting the variables needed for Figure 2
figure2_data <- data_raw %>%
  select(
    workerid,
    verb,
    response,
    probe_text
  )

#Making sure response is numeric
figure2_data <- figure2_data %>%
  mutate(
    response = as.numeric(response)
  )

#Removing rows without a response, predicate, or participant ID
figure2_data <- figure2_data %>%
  filter(
    !is.na(response),
    !is.na(verb),
    !is.na(workerid)
  )

#Changing inform_Sam to inform
figure2_data <- figure2_data %>%
  mutate(
    verb = recode(
      verb,
      "inform_Sam" = "inform"
    )
  )

#Categorizing predicates by predicate type
figure2_data <- figure2_data %>%
  mutate(
    predicate_type = case_when(
      
      verb == "control" ~
        "Control",
      
      verb %in% c(
        "think",
        "suggest",
        "say"
      ) ~
        "Nonfactive",
      
      verb %in% c(
        "prove",
        "confirm",
        "establish",
        "acknowledge",
        "hear",
        "inform"
      ) ~
        "Optionally factive",
      
      verb %in% c(
        "discover",
        "know",
        "reveal"
      ) ~
        "Canonically factive",
      
      TRUE ~ NA_character_
    )
  )

#Coding boundedness from the probe_text column
figure2_data <- figure2_data %>%
  mutate(
    boundedness = case_when(
      
      #The main-clause control has no boundedness manipulation
      verb == "control" ~
        "Control",
      
      #Upper-bounded probes
      str_detect(
        probe_text,
        regex(
          "but not both|but not all of them",
          ignore_case = TRUE
        )
      ) ~
        "Upper-bounded",
      
      #Lower-bounded probes
      str_detect(
        probe_text,
        regex(
          "and possibly both|and possibly all of them",
          ignore_case = TRUE
        )
      ) ~
        "Lower-bounded",
      
      TRUE ~ NA_character_
    )
  )

#Removing predicates or probes that were not successfully categorized
figure2_data <- figure2_data %>%
  filter(
    !is.na(predicate_type),
    !is.na(boundedness)
  )

#Checking the boundedness coding
figure2_data %>%
  count(
    boundedness,
    probe_text
  ) %>%
  print(
    n = Inf
  )

#Calculating each participant's mean rating
#for each predicate and boundedness condition
participant_boundedness_data <- figure2_data %>%
  group_by(
    workerid,
    verb,
    predicate_type,
    boundedness
  ) %>%
  summarise(
    participant_rating = mean(
      response,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

#Calculating the overall mean for each predicate and boundedness condition
boundedness_summary <- participant_boundedness_data %>%
  group_by(
    verb,
    predicate_type,
    boundedness
  ) %>%
  summarise(
    mean_certainty = mean(
      participant_rating,
      na.rm = TRUE
    ),
    
    number_of_participants = n(),
    
    .groups = "drop"
  )

#Making the bootstrap results reproducible
set.seed(2026)

#Calculating 95% bootstrap confidence intervals
#for each predicate and boundedness condition
boundedness_bootstrap_ci <- participant_boundedness_data %>%
  group_by(
    verb,
    predicate_type,
    boundedness
  ) %>%
  group_modify(
    ~ bootstrap_mean_ci(
      .x$participant_rating,
      number_of_bootstraps = 5000
    )
  ) %>%
  ungroup()

#Combining means and confidence intervals
boundedness_summary <- boundedness_summary %>%
  left_join(
    boundedness_bootstrap_ci,
    by = c(
      "verb",
      "predicate_type",
      "boundedness"
    )
  )

#Creating the predicate order based only on upper-bounded means
upper_bounded_order <- boundedness_summary %>%
  filter(
    boundedness == "Upper-bounded"
  ) %>%
  arrange(
    mean_certainty
  ) %>%
  pull(
    verb
  ) %>%
  as.character()

#Putting the main-clause control first
figure2_predicate_order <- c(
  "control",
  upper_bounded_order
)

#Removing possible duplicate predicate names
figure2_predicate_order <- unique(
  figure2_predicate_order
)

#Applying the order to participant-level data
participant_boundedness_data <- participant_boundedness_data %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = figure2_predicate_order
    )
  )

#Applying the same order to summary data
boundedness_summary <- boundedness_summary %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = figure2_predicate_order
    )
  )

#Creating a color category for the plotted points
boundedness_summary <- boundedness_summary %>%
  mutate(
    point_group = case_when(
      
      boundedness == "Control" ~
        "Main clause controls",
      
      boundedness == "Lower-bounded" ~
        "Lower-bounded",
      
      boundedness == "Upper-bounded" &
        predicate_type == "Nonfactive" ~
        "Upper-bounded: nonfactive",
      
      boundedness == "Upper-bounded" &
        predicate_type == "Optionally factive" ~
        "Upper-bounded: optionally factive",
      
      boundedness == "Upper-bounded" &
        predicate_type == "Canonically factive" ~
        "Upper-bounded: factive",
      
      TRUE ~ NA_character_
    )
  )

#Checking the final predicate order
levels(
  participant_boundedness_data$verb
)

#Checking the means before plotting
boundedness_summary %>%
  arrange(
    verb,
    boundedness
  ) %>%
  select(
    verb,
    boundedness,
    predicate_type,
    mean_certainty,
    lower_ci,
    upper_ci
  ) %>%
  print(
    n = Inf
  )

#Creating Figure 2
figure2 <- ggplot(
  participant_boundedness_data,
  aes(
    x = verb,
    y = participant_rating
  )
) +
  
  #Showing one participant-level distribution for each predicate
  geom_violin(
    aes(
      group = verb
    ),
    fill = "white",
    color = "grey80",
    linewidth = 0.6,
    width = 0.85,
    scale = "width",
    trim = FALSE
  ) +
  
  #Showing the 95% bootstrap confidence intervals
  #for control, upper-bounded, and lower-bounded means
  geom_errorbar(
    data = boundedness_summary,
    aes(
      x = verb,
      ymin = lower_ci,
      ymax = upper_ci,
      color = point_group
    ),
    inherit.aes = FALSE,
    width = 0.03,
    linewidth = 0.3
  ) +
  
  #Showing the control, upper-bounded, and lower-bounded means
  #Showing lower-bounded means (smaller)
  geom_point(
    data = boundedness_summary %>%
      filter(
        boundedness == "Lower-bounded"
      ),
    aes(
      x = verb,
      y = mean_certainty,
      color = point_group,
      shape = predicate_type
    ),
    inherit.aes = FALSE,
    size = 1.5
  ) +
  
  #Showing control and upper-bounded means (larger)
  geom_point(
    data = boundedness_summary %>%
      filter(
        boundedness != "Lower-bounded"
      ),
    aes(
      x = verb,
      y = mean_certainty,
      color = point_group,
      shape = predicate_type
    ),
    inherit.aes = FALSE,
    size = 3.0
  ) +
  
  #Upper-bounded colors match Figure 1
  #All lower-bounded means are shown in green
  scale_color_manual(
    values = c(
      "Main clause controls" = "black",
      "Upper-bounded: nonfactive" = "grey55",
      "Upper-bounded: optionally factive" = "#F15A3A",
      "Upper-bounded: factive" = "#9C39C6",
      "Lower-bounded" = "#2E8B57"
    ),
    
    breaks = c(
      "Main clause controls",
      "Upper-bounded: nonfactive",
      "Upper-bounded: optionally factive",
      "Upper-bounded: factive",
      "Lower-bounded"
    )
  ) +
  
  #Using the same predicate-type shapes as Figure 1
  scale_shape_manual(
    values = c(
      "Control" = 16,
      "Nonfactive" = 15,
      "Optionally factive" = 17,
      "Canonically factive" = 18
    ),
    
    breaks = c(
      "Control",
      "Nonfactive",
      "Optionally factive",
      "Canonically factive"
    ),
    
    labels = c(
      "Control" = "main clause controls",
      "Nonfactive" = "nonfactive",
      "Optionally factive" = "optionally factive",
      "Canonically factive" = "factive"
    )
  ) +
  
  #Forcing the intended x-axis order
  #and displaying control as MC
  scale_x_discrete(
    limits = figure2_predicate_order,
    
    labels = function(x) {
      ifelse(
        x == "control",
        "MC",
        x
      )
    }
  ) +
  
  #Showing certainty values from 0 to 1
  scale_y_continuous(
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    
    expand = expansion(
      mult = c(
        0.01,
        0.03
      )
    )
  ) +
  
  #Displaying the response range without removing violin density
  coord_cartesian(
    ylim = c(
      0,
      1
    )
  ) +
  
  #Adding axis and legend labels
  labs(
    x = "Predicate",
    y = "Mean certainty rating",
    color = "Boundedness and predicate type",
    shape = "Predicate type",
    caption = paste(
      "Figure 2. Mean certainty ratings by predicate and boundedness condition."
    )
  ) +
  
  #Using the same general style as Figure 1
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 50,
      hjust = 1,
      vjust = 1,
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      plot.caption = element_text(size = 9),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 11),
      legend.spacing.x = unit(0.2, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.key.width = unit(0.7, "cm"),
      legend.key.height = unit(0.5, "cm")
    ),
    
    legend.position = "bottom",
    
    legend.box = "vertical",
    plot.caption = element_text(
      hjust = 0,
      size = 11,
      face = "plain",
      margin = margin(
        t = 15
      )
    ),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.6
    )
  ) +
  
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 2,
      order = 1
    ),
    
    shape = guide_legend(
      title.position = "top",
      nrow = 1,
      order = 2
    )
  )

#Displaying Figure 2
figure2

ggsave(
  filename = "Figure 2. Mean certainty ratings by predicate and boundedness condition.png",,
  width = 11,
  height = 8,
  dpi = 300
)