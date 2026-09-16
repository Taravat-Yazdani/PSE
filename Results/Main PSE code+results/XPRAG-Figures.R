# ============================================================
# Xprag Figure 1: Predicate means separated by scalar expression
# Final size: 11.71 inches wide x 5.25 inches high
# ============================================================

library(tidyverse)
library(cowplot)

data_raw <- read_csv(
  "/Users/taravat/Desktop/QP/QP/Data/PSE-Main data/Cleaned data/PSE-merged.csv",
  show_col_types = FALSE
) %>%
  mutate(
    participant_id = paste(
      workerid,
      time_in_minutes,
      sep = "_"
    ),
    
    verb = trigger
  )

bootstrap_mean_ci <- function(
    values,
    number_of_bootstraps = 5000
) {
  
  values <- values[!is.na(values)]
  
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

figure1_data <- data_raw %>%
  select(
    participant_id,
    verb,
    response,
    scale
  ) %>%
  mutate(
    response = as.numeric(response),
    
    verb = recode(
      verb,
      "inform_Sam" = "inform"
    ),
    
    scale_label = case_when(
      scale == "some" ~ "some (quantifier)",
      scale == "or" ~ "or (disjunction)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(participant_id),
    !is.na(verb),
    !is.na(response)
  )

# One violin per predicate + MC
violin_data <- figure1_data %>%
  group_by(
    participant_id,
    verb
  ) %>%
  summarise(
    participant_rating = mean(response),
    .groups = "drop"
  )

# Separate some and or means for every embedding predicate
participant_predicate_scale_data <- figure1_data %>%
  filter(
    verb != "control",
    !is.na(scale_label)
  ) %>%
  group_by(
    participant_id,
    verb,
    scale_label
  ) %>%
  summarise(
    participant_rating = mean(response),
    .groups = "drop"
  )

predicate_scale_summary <- participant_predicate_scale_data %>%
  group_by(
    verb,
    scale_label
  ) %>%
  summarise(
    mean_certainty = mean(participant_rating),
    .groups = "drop"
  )

set.seed(2026)

predicate_scale_bootstrap_ci <- participant_predicate_scale_data %>%
  group_by(
    verb,
    scale_label
  ) %>%
  group_modify(
    ~ bootstrap_mean_ci(.x$participant_rating)
  ) %>%
  ungroup()

predicate_scale_summary <- predicate_scale_summary %>%
  left_join(
    predicate_scale_bootstrap_ci,
    by = c(
      "verb",
      "scale_label"
    )
  )

# MC benchmark summary
participant_control_data <- figure1_data %>%
  filter(verb == "control") %>%
  group_by(
    participant_id,
    verb
  ) %>%
  summarise(
    participant_rating = mean(response),
    .groups = "drop"
  )

control_summary <- participant_control_data %>%
  group_by(verb) %>%
  summarise(
    mean_certainty = mean(participant_rating),
    .groups = "drop"
  )

set.seed(2026)

control_bootstrap_ci <- participant_control_data %>%
  group_by(verb) %>%
  group_modify(
    ~ bootstrap_mean_ci(.x$participant_rating)
  ) %>%
  ungroup()

control_summary <- control_summary %>%
  left_join(
    control_bootstrap_ci,
    by = "verb"
  )

# Order predicates by their combined mean certainty rating
predicate_order <- violin_data %>%
  group_by(verb) %>%
  summarise(
    mean_certainty = mean(participant_rating),
    .groups = "drop"
  ) %>%
  arrange(mean_certainty) %>%
  pull(verb) %>%
  as.character()

violin_data <- violin_data %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = predicate_order
    )
  )

predicate_scale_summary <- predicate_scale_summary %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = predicate_order
    ),
    
    scale_label = factor(
      scale_label,
      levels = c(
        "some (quantifier)",
        "or (disjunction)"
      )
    )
  )

control_summary <- control_summary %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = predicate_order
    )
  )



main_plot <- ggplot(
  violin_data,
  aes(
    x = verb,
    y = participant_rating
  )
) +
  
  geom_violin(
    aes(group = verb),
    fill = "white",
    color = "grey72",
    linewidth = 0.7,
    width = 0.82,
    scale = "width",
    trim = FALSE
  ) +
  
  # Confidence intervals for some and or:
  geom_errorbar(
    data = predicate_scale_summary,
    aes(
      x = verb,
      ymin = lower_ci,
      ymax = upper_ci,
      color = scale_label
    ),
    inherit.aes = FALSE,
    width = 0.10,
    linewidth = 0.8
  ) +
  
  # Orange and purple dots directly above one another
  geom_point(
    data = predicate_scale_summary,
    aes(
      x = verb,
      y = mean_certainty,
      color = scale_label
    ),
    inherit.aes = FALSE,
    size = 4.2
  ) +
  
  geom_errorbar(
    data = control_summary,
    aes(
      x = verb,
      ymin = lower_ci,
      ymax = upper_ci
    ),
    inherit.aes = FALSE,
    color = "black",
    width = 0.10,
    linewidth = 0.8
  ) +
  
  geom_point(
    data = control_summary,
    aes(
      x = verb,
      y = mean_certainty
    ),
    inherit.aes = FALSE,
    color = "black",
    size = 4.2
  ) +
  
  scale_color_manual(
    values = c(
      "some (quantifier)" = "#B22222",
      "or (disjunction)" = "#1F4E8C"
    ),
    
    breaks = c(
      "some (quantifier)",
      "or (disjunction)"
    )
  ) +
  
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
  
  coord_cartesian(
    ylim = c(
      0,
      1
    )
  ) +
  
  labs(
    x = "Predicate",
    y = "Mean certainty rating",
    color = NULL
  ) +
  
  theme_classic(
    base_size = 18
  ) +
  
  theme(
    axis.title = element_text(
      size = 19,
      face = "bold"
    ),
    
    axis.title.x = element_text(
      margin = margin(
        t = 5,
        b = 0
      )
    ),
    
    axis.text.x = element_text(
      size = 16,
      face = "bold",
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    
    axis.text.y = element_text(
      size = 16
    ),
    
    legend.position = "none",
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    plot.margin = margin(
      t = 8,
      r = 10,
      b = 0,
      l = 10
    )
  )

# ---------------------
# Caption and legend 
# ----------------------

legend_grob <- cowplot::get_legend(
  main_plot +
    theme(
      legend.position = "bottom",
      
      legend.direction = "horizontal",
      
      legend.text = element_text(
        size = 13
      ),
      
      legend.key.width = grid::unit(
        0.45,
        "cm"
      ),
      
      legend.key.height = grid::unit(
        0.35,
        "cm"
      ),
      
      legend.spacing.x = grid::unit(
        0.12,
        "cm"
      ),
      
      legend.margin = margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      )
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    )
)

caption_grob <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Mean certainty ratings by predicate and scale (some vs. or).",
    x = 0,
    y = 0.5,
    hjust = 0,
    vjust = 0.5,
    size = 14
  )

# Caption on left; legend on right
footer <- cowplot::plot_grid(
  caption_grob,
  legend_grob,
  nrow = 1,
  rel_widths = c(
    1.35,
    1
  )
)

# Final centred figure
figure1_poster <- cowplot::plot_grid(
  main_plot,
  footer,
  ncol = 1,
  rel_heights = c(
    1,
    0.10
  )
)

print(figure1_poster)

ggsave(
  filename = "/Users/taravat/Desktop/Xprag Figure 1 Predicate by Scale.pdf",
  plot = figure1_poster,
  width = 10,
  height = 5,
  units = "in",
  device = "pdf",
  useDingbats = FALSE
)

# ============================================================
# Xprag Figure 2: Predicate means by boundedness condition
# Final size: 11.71 inches wide x 5.25 inches high
# ============================================================
data_raw <- read_csv(
  "/Users/taravat/Desktop/QP/QP/Data/PSE-Main data/Cleaned data/PSE-merged.csv",
  show_col_types = FALSE
) %>%
  mutate(
    participant_id = paste(
      workerid,
      time_in_minutes,
      sep = "_"
    ),
    
    verb = trigger
  )

# Function to calculate 95% bootstrapped confidence intervals
bootstrap_mean_ci <- function(
    values,
    number_of_bootstraps = 5000
) {
  
  values <- values[!is.na(values)]
  
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



figure2_data <- data_raw %>%
  select(
    participant_id,
    verb,
    response,
    condition,
    scale
  ) %>%
  mutate(
    response = as.numeric(response),
    
    verb = recode(
      verb,
      "inform_Sam" = "inform"
    ),
    
    predicate_type = case_when(
      verb == "control" ~ "Control",
      
      verb %in% c(
        "think",
        "suggest",
        "say"
      ) ~ "Nonfactive",
      
      verb %in% c(
        "prove",
        "confirm",
        "establish",
        "acknowledge",
        "hear",
        "inform"
      ) ~ "Optionally factive",
      
      verb %in% c(
        "discover",
        "know",
        "reveal"
      ) ~ "Canonically factive",
      
      TRUE ~ NA_character_
    ),
    
    boundedness = case_when(
      verb == "control" ~ "Control",
      condition == "ub" ~ "Upper-bounded",
      condition == "lb" ~ "Lower-bounded",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(response),
    !is.na(verb),
    !is.na(participant_id),
    !is.na(predicate_type),
    !is.na(boundedness)
  )

participant_boundedness_data <- figure2_data %>%
  group_by(
    participant_id,
    verb,
    predicate_type,
    boundedness
  ) %>%
  summarise(
    participant_rating = mean(response),
    .groups = "drop"
  )

boundedness_summary <- participant_boundedness_data %>%
  group_by(
    verb,
    predicate_type,
    boundedness
  ) %>%
  summarise(
    mean_certainty = mean(participant_rating),
    .groups = "drop"
  )

set.seed(2026)

boundedness_bootstrap_ci <- participant_boundedness_data %>%
  group_by(
    verb,
    predicate_type,
    boundedness
  ) %>%
  group_modify(
    ~ bootstrap_mean_ci(.x$participant_rating)
  ) %>%
  ungroup()

boundedness_summary <- boundedness_summary %>%
  left_join(
    boundedness_bootstrap_ci,
    by = c(
      "verb",
      "predicate_type",
      "boundedness"
    )
  )

# Put MC first; order other predicates by upper-bounded means
upper_bounded_order <- boundedness_summary %>%
  filter(
    boundedness == "Upper-bounded"
  ) %>%
  arrange(mean_certainty) %>%
  pull(verb) %>%
  as.character()

figure2_predicate_order <- unique(
  c(
    "control",
    upper_bounded_order
  )
)

participant_boundedness_data <- participant_boundedness_data %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = figure2_predicate_order
    )
  )

boundedness_summary <- boundedness_summary %>%
  mutate(
    verb = factor(
      as.character(verb),
      levels = figure2_predicate_order
    ),
    
    point_group = case_when(
      boundedness == "Control" ~ "MC control",
      
      boundedness == "Lower-bounded" ~ "Lower-bounded",
      
      boundedness == "Upper-bounded" &
        predicate_type == "Nonfactive" ~ "Upper: nonfactive",
      
      boundedness == "Upper-bounded" &
        predicate_type == "Optionally factive" ~ "Upper: optional",
      
      boundedness == "Upper-bounded" &
        predicate_type == "Canonically factive" ~ "Upper: factive"
    )
  )

legend_spacer_data <- tibble(
  verb = factor(
    figure2_predicate_order[1],
    levels = figure2_predicate_order
  ),
  
  mean_certainty = 0,
  
  point_group = "spacer"
)



main_plot2 <- ggplot(
  participant_boundedness_data,
  aes(
    x = verb,
    y = participant_rating
  )
) +
  
  geom_violin(
    aes(group = verb),
    fill = "white",
    color = "grey72",
    linewidth = 0.7,
    width = 0.82,
    scale = "width",
    trim = FALSE
  ) +
  
  geom_errorbar(
    data = boundedness_summary,
    aes(
      x = verb,
      ymin = lower_ci,
      ymax = upper_ci,
      color = point_group
    ),
    inherit.aes = FALSE,
    width = 0.10,
    linewidth = 0.8
  ) +
  
  geom_point(
    data = boundedness_summary,
    aes(
      x = verb,
      y = mean_certainty,
      color = point_group
    ),
    inherit.aes = FALSE,
    size = 3.8
  ) +
  
  geom_point(
    data = legend_spacer_data,
    aes(
      x = verb,
      y = mean_certainty,
      color = point_group
    ),
    inherit.aes = FALSE,
    alpha = 0,
    size = 3.8,
    show.legend = TRUE
  ) +
  
  scale_color_manual(
    values = c(
      "MC control" = "black",
      "Lower-bounded" = "#2E8B57",
      "spacer" = "white",
      "Upper: nonfactive" = "grey40",
      "Upper: optional" = "#F15A3A",
      "Upper: factive" = "#9C39C6"
    ),
    
    breaks = c(
      "MC control",
      "Lower-bounded",
      "spacer",
      "Upper: nonfactive",
      "Upper: optional",
      "Upper: factive"
    ),
    
    labels = c(
      "MC control" = "MC control",
      "Lower-bounded" = "Lower-bounded",
      "spacer" = "",
      "Upper: nonfactive" = "UB: nonfactive",
      "Upper: optional" = "UB: optionally factive",
      "Upper: factive" = "UB: factive"
    )
  ) +
  
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
  
  coord_cartesian(
    ylim = c(
      0,
      1
    )
  ) +
  
  labs(
    x = "Predicate",
    y = "Mean certainty rating",
    color = NULL
  ) +
  
  theme_classic(
    base_size = 18
  ) +
  
  theme(
    axis.title = element_text(
      size = 19,
      face = "bold"
    ),
    
    axis.title.x = element_text(
      margin = margin(
        t = 5,
        b = 0
      )
    ),
    
    axis.text.x = element_text(
      size = 16,
      face = "bold",
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    
    axis.text.y = element_text(
      size = 16
    ),
    
    legend.position = "none",
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    plot.margin = margin(
      t = 8,
      r = 10,
      b = 0,
      l = 10
    )
  )


legend_grob2 <- cowplot::get_legend(
  main_plot2 +
    theme(
      legend.position = "bottom",
      
      legend.direction = "horizontal",
      
      legend.text = element_text(
        size = 13
      ),
      
      legend.key.width = grid::unit(
        0.42,
        "cm"
      ),
      
      legend.key.height = grid::unit(
        0.35,
        "cm"
      ),
      
      legend.spacing.x = grid::unit(
        0.10,
        "cm"
      ),
      
      legend.margin = margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      )
    ) +
    guides(
      color = guide_legend(
        nrow = 2,
        byrow = TRUE
      )
    )
)

caption_grob2 <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Mean certainty ratings by predicate and boundedness.",
    x = 0,
    y = 0.5,
    hjust = 0,
    vjust = 0.5,
    size = 14
  )

footer2 <- cowplot::plot_grid(
  caption_grob2,
  legend_grob2,
  nrow = 1,
  rel_widths = c(
    1.0,
    1.8
  )
)

# Final Figure 2
figure2_poster <- cowplot::plot_grid(
  main_plot2,
  footer2,
  ncol = 1,
  rel_heights = c(
    1,
    0.14
  )
)

print(figure2_poster)

ggsave(
  filename = "/Users/taravat/Desktop/Xprag Figure 2 Boundedness.pdf",
  plot = figure2_poster,
  width = 10,
  height = 5,
  units = "in",
  device = "pdf",
  useDingbats = FALSE
)

