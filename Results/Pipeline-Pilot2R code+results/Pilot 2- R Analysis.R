#Installing and calling necessary packages
install.packages(c("tidyverse", "patchwork"))
library(tidyverse)
library(patchwork)

#Reading the merged file 
data_raw <- read_csv(
  "/Users/taravat/Desktop/QP/QP/Data/Pipeline-Pilot2 Data/PSE-merged.csv"
)

# Creating verb from the trigger column
data_raw <- data_raw %>%
  mutate(
    verb = trigger
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
    shape = "Predicate type",
    x = "Predicate",
    y = "Mean certainty rating",
    color = "Predicate type",
    shape = "Predicate type",
    caption = "Figure 1. Mean certainty rating by predicate."
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
    #Left-aligning the caption below the legend
    plot.caption.position = "plot",
    
    plot.caption = element_text(
      hjust = 0,
      size = 11,
      face = "plain",
      margin = margin(
        t = 12
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
    condition,
    scale
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

#Coding boundedness from the condition column
figure2_data <- figure2_data %>%
  mutate(
    boundedness = case_when(
      verb == "control" ~ "Control",
      condition == "ub" ~ "Upper-bounded",
      condition == "lb" ~ "Lower-bounded",
      TRUE ~ NA_character_
    )
  )

#Removing predicates or probes that were not successfully categorized
figure2_data <- figure2_data %>%
  filter(
    !is.na(predicate_type),
    !is.na(boundedness)
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
  filename = "Figure 2. Mean certainty ratings by predicate and boundedness condition.png",
  width = 11,
  height = 8,
  dpi = 300
)



#-------------------------------------------------------------------
# Figures 3 & 4:
# Mean certainty by predicate, boundedness, and scale
#-------------------------------------------------------------------

# 1. 

figure34_data <- figure2_data %>%
  transmute(
    workerid,
    verb = as.character(verb),
    response = as.numeric(response),
    predicate_type,
    
    # Using scale directly from the new CSV
    scale = case_when(
      verb == "control" ~ "Control",
      scale == "or" ~ "Disjunction",
      scale == "some" ~ "Some",
      TRUE ~ NA_character_
    ),
    
    # Using condition directly from the new CSV
    boundedness = case_when(
      verb == "control" ~ "Control",
      condition == "ub" ~ "Upper-bounded",
      condition == "lb" ~ "Lower-bounded",
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(
    !is.na(workerid),
    !is.na(verb),
    !is.na(response),
    !is.na(predicate_type),
    !is.na(scale),
    !is.na(boundedness)
  ) %>%
  
  mutate(
    scale = factor(
      scale,
      levels = c(
        "Control",
        "Disjunction",
        "Some"
      )
    ),
    
    boundedness = factor(
      boundedness,
      levels = c(
        "Control",
        "Upper-bounded",
        "Lower-bounded"
      )
    )
  )

#-------------------------------------------------------------------
# Function for preparing and plotting one scale
#-------------------------------------------------------------------
#
#This function performs the same operations for both scales:
#1. Keeps the selected scale and the controls
#2. Calculates participant-level means
#3. Calculates condition means
#4. Calculates bootstrapped confidence intervals
#5. Orders predicates by the upper-bounded mean
#6. Creates the violin plot

make_boundedness_scale_plot <- function(
    data,
    selected_scale,
    figure_number,
    caption_scale
) {
  
  #---------------------------------------------------------------
  #Keeping one scale and the main-clause controls
  #---------------------------------------------------------------
  
  scale_plot_data <- data %>%
    filter(
      scale %in% c(
        "Control",
        selected_scale
      )
    )
  
  
  #---------------------------------------------------------------
  #Calculating participant-level means
  #---------------------------------------------------------------
  
  participant_scale_data <- scale_plot_data %>%
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
  
  
  #---------------------------------------------------------------
  #Calculating the overall mean for each condition
  #---------------------------------------------------------------
  
  scale_summary <- participant_scale_data %>%
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
      
      number_of_participants = n_distinct(
        workerid
      ),
      
      .groups = "drop"
    )
  
  
  #---------------------------------------------------------------
  #Calculating 95% bootstrapped confidence intervals
  #---------------------------------------------------------------
  
  #Using a different but reproducible seed for each figure
  set.seed(
    2026 + figure_number
  )
  
  scale_bootstrap_ci <- participant_scale_data %>%
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
  
  
  #Combining the means and confidence intervals
  scale_summary <- scale_summary %>%
    left_join(
      scale_bootstrap_ci,
      by = c(
        "verb",
        "predicate_type",
        "boundedness"
      )
    )
  
  
  #---------------------------------------------------------------
  #Ordering predicates by their upper-bounded means
  #---------------------------------------------------------------
  
  #Extracting the upper-bounded mean for each non-control predicate
  upper_mean_table <- scale_summary %>%
    filter(
      verb != "control",
      boundedness == "Upper-bounded"
    ) %>%
    select(
      verb,
      upper_mean = mean_certainty
    )
  
  
  #Starting with every predicate that appears for this scale
  predicate_order_table <- scale_plot_data %>%
    filter(
      verb != "control"
    ) %>%
    distinct(
      verb
    ) %>%
    
    #Adding the upper-bounded mean when it is available
    left_join(
      upper_mean_table,
      by = "verb"
    ) %>%
    
    #Predicates with an observed upper-bounded mean are ordered from lowest to highest.
    #
    #Predicates without an upper-bounded observation are put last
    #because they cannot be ordered by an unavailable mean.
    arrange(
      is.na(upper_mean),
      upper_mean,
      verb
    )
  
  
  #Finding predicates that do not currently have
  #an upper-bounded observation
  missing_upper_predicates <- predicate_order_table %>%
    filter(
      is.na(upper_mean)
    ) %>%
    pull(
      verb
    ) %>%
    as.character()
  
  
  #Printing an informative message when pilot cells are missing
  if (
    length(missing_upper_predicates) > 0
  ) {
    
    message(
      paste0(
        "For the ",
        selected_scale,
        " scale, no upper-bounded observations were available for: ",
        paste(
          missing_upper_predicates,
          collapse = ", "
        ),
        ". These predicates are placed after predicates with observed ",
        "upper-bounded means."
      )
    )
  }
  
  
  #The control is placed first as the benchmark, and the remaining predicates are ordered by upper-bounded mean.
  predicate_order <- c(
    "control",
    predicate_order_table$verb
  )
  
  
  #Removing any possible duplicate names
  predicate_order <- unique(
    predicate_order
  )
  
  
  #Applying the predicate order to participant-level data
  participant_scale_data <- participant_scale_data %>%
    mutate(
      verb = factor(
        as.character(verb),
        levels = predicate_order
      )
    )
  
  
  #Applying the same predicate order to summary data
  scale_summary <- scale_summary %>%
    mutate(
      verb = factor(
        as.character(verb),
        levels = predicate_order
      )
    )
  
  
  #---------------------------------------------------------------
  #Creating color groups for the plotted points
  #---------------------------------------------------------------
  
  scale_summary <- scale_summary %>%
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
  
  
  #---------------------------------------------------------------
  #Creating the violin plot
  #---------------------------------------------------------------
  
  scale_plot <- ggplot(
    participant_scale_data,
    aes(
      x = verb,
      y = participant_rating
    )
  ) +
    
    #One pooled participant-level distribution per predicate
    #
    #Upper- and lower-bounded participant ratings for that scale
    #are included in the same violin.
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
    
    #Confidence intervals for control, upper-bounded,
    #and lower-bounded means
    geom_errorbar(
      data = scale_summary,
      aes(
        x = verb,
        ymin = lower_ci,
        ymax = upper_ci,
        color = point_group
      ),
      inherit.aes = FALSE,
      width = 0.03,
      linewidth = 0.35
    ) +
    
    #Lower-bounded means:
    #green, with predicate-type shapes
    geom_point(
      data = scale_summary %>%
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
      size = 2
    ) +
    
    #Upper-bounded means and the control:
    #larger points with Figure 1 and Figure 2 colors
    geom_point(
      data = scale_summary %>%
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
      size = 3
    ) +
    
    #Upper-bounded colors correspond to predicate type.
    #All lower-bounded means are green.
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
      ),
      
      name = "Boundedness and predicate type"
    ) +
    
    #Shapes remain constant across upper- and lower-bounded means
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
      ),
      
      name = "Predicate type"
    ) +
    
    #Forcing the selected predicate order and displaying the control as MC
    scale_x_discrete(
      limits = predicate_order,
      
      labels = function(x) {
        ifelse(
          x == "control",
          "MC",
          x
        )
      },
      
      drop = FALSE
    ) +
    
    #Showing certainty values between 0 and 1
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
    
    #Displaying the response range without deleting the violin density outside the plotting window
    coord_cartesian(
      ylim = c(
        0,
        1
      )
    ) +
    
    #Axis labels and a separate caption for each plot
    labs(
      x = "Predicate",
      y = "Mean certainty rating",
      
      caption = paste0(
        "Figure ",
        figure_number,
        ". Mean certainty ratings by predicate and boundedness ",
        "for the ",
        caption_scale,
        " scale."
      )
    ) +
    
    theme_classic() +
    
    theme(
      axis.text.x = element_text(
        angle = 50,
        hjust = 1,
        vjust = 1
      ),
      
      axis.title = element_text(
        size = 13
      ),
      
      axis.text = element_text(
        size = 10
      ),
      
      legend.title = element_text(
        size = 10
      ),
      
      legend.text = element_text(
        size = 9
      ),
      
      legend.position = "bottom",
      
      legend.box = "vertical",
      
      legend.spacing.x = grid::unit(
        0.2,
        "cm"
      ),
      
      legend.spacing.y = grid::unit(
        0.1,
        "cm"
      ),
      
      legend.key.width = grid::unit(
        0.8,
        "cm"
      ),
      
      legend.key.height = grid::unit(
        0.55,
        "cm"
      ),
      
      #Ensuring that each caption appears beneath its own plot
      plot.caption.position = "plot",
      
      plot.caption = element_text(
        hjust = 0,
        size = 10,
        face = "plain",
        margin = margin(
          t = 12,
          b = 8
        )
      ),
      
      #Adding space around every individual plot
      plot.margin = margin(
        t = 12,
        r = 18,
        b = 16,
        l = 18
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
  
  
  #Returning the plot and the datasets used to construct it
  list(
    plot = scale_plot,
    participant_data = participant_scale_data,
    summary = scale_summary,
    predicate_order = predicate_order,
    missing_upper_predicates = missing_upper_predicates
  )
}


#-------------------------------------------------------------------
# 4. Figure 3: Disjunction
#-------------------------------------------------------------------

figure3_results <- make_boundedness_scale_plot(
  data = figure34_data,
  selected_scale = "Disjunction",
  figure_number = 3,
  caption_scale = "disjunction"
)

figure3 <- figure3_results$plot


#Displaying Figure 3 separately
figure3


#-------------------------------------------------------------------
# 5. Figure 4: Quantifier 'some'
#-------------------------------------------------------------------

figure4_results <- make_boundedness_scale_plot(
  data = figure34_data,
  selected_scale = "Some",
  figure_number = 4,
  caption_scale = "quantifier 'some'"
)

figure4 <- figure4_results$plot

#Displaying Figure 4 separately
figure4


#-------------------------------------------------------------------
# 6. Placing Figures 3 and 4 vertically in one image
#-------------------------------------------------------------------

#Combining the two figures vertically
#and reserving a separate bottom row for the shared legend
figure3_and_figure4 <- patchwork::wrap_plots(
  
  plots = list(
    
    #Figure 3 appears at the top
    figure3,
    
    #Figure 4 appears underneath Figure 3
    figure4,
    
    #A separate area at the bottom for the collected legend
    patchwork::guide_area()
  ),
  
  #One item per row
  ncol = 1,
  
  #Equal space for the two figures and a smaller space
  #for the shared legend
  heights = c(
    1,
    1,
    0.18
  ),
  
  #Collecting the identical legends from Figure 3 and Figure 4
  guides = "collect"
) &
  
  #Applying this formatting to the combined figure
  ggplot2::theme(
    
    #Placing the collected legend in the bottom guide area
    legend.position = "bottom",
    
    #Arranging the color and shape legends vertically
    legend.box = "vertical",
    
    #Centering the complete legend
    legend.box.just = "center",
    
    #Keeping the individual captions close to their figures
    plot.margin = ggplot2::margin(
      t = 12,
      r = 20,
      b = 5,
      l = 20
    )
  )


#Displaying the combined figure
figure3_and_figure4


#-------------------------------------------------------------------
# 7. Saving Figures 3 and 4 in one PNG file
#-------------------------------------------------------------------

#Folder where the final figure should be saved
output_folder <- "/Users/taravat/Desktop/QP/QP/Data/Pipeline-Pilot2R code+results"

#Creating the complete output filename
output_file <- file.path(
  output_folder,
  paste0(
    "Figures 3 and 4 - Mean certainty by predicate, ",
    "boundedness, and scale.png"
  )
)

#Saving a high-resolution PNG
ggplot2::ggsave(
  filename = output_file,
  plot = figure3_and_figure4,
  width = 12,
  height = 18,
  units = "in",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)


#Printing the exact saved-file location
cat(
  "The combined figure was saved here:\n",
  normalizePath(
    output_file,
    mustWork = TRUE
  ),
  "\n"
)