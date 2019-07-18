# Library -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)

# Import ------------------------------------------------------------------

plot_data <-
  readRDS('data/tutorial_plot_data.rds')


# P-Value transformation --------------------------------------------------

plot_data <-
  plot_data %>%
  mutate(p.value = -1 * log10(p.value))


# Extreme Threshold -------------------------------------------------------
pv_threshold <- 15

plot_data <-
  plot_data %>%
  mutate(extreme = p.value > pv_threshold)

plot_data <-
  plot_data %>%
  mutate(label = ifelse(extreme, term, NA))


# Manual Term Position Calculation ----------------------------------------


n_terms <-
  n_distinct(plot_data$term)

# Space between sets of results
buffer <- n_terms/4

model_set_spacing <-
  plot_data %>%
  distinct(response) %>%
  # Assign each set of results to a block
  mutate(block = 1:n() - 1) %>%
  # Calculate the center location of each set of results
  mutate(center = block * n_terms + block * buffer + median(1:n_terms)) %>%
  # Assign each set of results to a color. Here, using alternating colors
  mutate(color_group = as.factor(rep_len(c(0, 1), length.out = n())))

# Calculate the position of each `term` relative to the median
# (median set to be (0)
term_position <-
  plot_data %>%
  distinct(term) %>%
  arrange(term) %>%
  mutate(x = 1:n() - 1) %>%
  mutate(x = x - median(x))

plot_data <-
  plot_data %>%
  left_join(model_set_spacing, by = 'response') %>%
  left_join(term_position, by = 'term')

# Combine relative positioning of each term with the center of each set of
# results to calculate each final term's positioning
plot_data <-
  plot_data %>%
  mutate(x = x + center)

# Theme -------------------------------------------------------------------

thm <-
  theme_light(base_size = 18) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Remove Vertical Grid Lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    # White background
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white')
  )

# Barebones Manhattan Plot ------------------------------------------------

manhattan_plot <-
  ggplot(plot_data) +
  geom_point(aes(x = term, y = p.value))

print(manhattan_plot)



# Manhattan Plot ----------------------------------------------------------

manhattan_plot <-
  ggplot(plot_data) +
  # Use the calculated x position and color
  geom_point(aes(x = x, y = p.value, color = color_group)) +
  # Add the first vertical line
  geom_vline(
    xintercept = -buffer/2,
    colour = 'lightblue',
    alpha = 0.5,
    linetype = 2
  ) +
  # Add every other one
  geom_vline(
    aes(xintercept = c(center + median(1:n_terms) + buffer/2)),
    colour = 'lightblue',
    alpha = 0.5,
    linetype = 2
  ) +
  scale_y_continuous(
    expression(paste(-log[10]('P-value'))),
    limits = c(0, NA),
    expand = expand_scale(mult = c(0, 0.05))) +
  # Use the x labels to mark the `response` each set of results corresponds to
  scale_x_continuous(
    breaks = model_set_spacing$center,
    labels = model_set_spacing$response
  ) +
  # Remove color legend
  scale_color_discrete(guide = FALSE) +
  thm

print(manhattan_plot)

# Point Size --------------------------------------------------------------

manhattan_plot_size <-
  ggplot(plot_data) +
  # Use the calculated x position and color
  geom_point(aes(x = x, y = p.value, color = color_group), size = 4) +
  # Add the first vertical line
  geom_vline(
    xintercept = -buffer/2,
    colour = 'lightblue',
    alpha = 0.5,
    linetype = 2
  ) +
  # Add every other one
  geom_vline(
    aes(xintercept = c(center + median(1:n_terms) + buffer/2)),
    colour = 'lightblue',
    alpha = 0.5,
    linetype = 2
  ) +
  scale_y_continuous(
    expression(paste(-log[10]('P-value'))),
    limits = c(0, NA),
    expand = expand_scale(mult = c(0, 0.05))) +
  # Use the x labels to mark the `response` each set of results corresponds to
  scale_x_continuous(
    breaks = model_set_spacing$center,
    labels = model_set_spacing$response
  ) +
  # Remove color legend
  scale_color_discrete(guide = FALSE) +
  thm

print(manhattan_plot_size)


# Values of Interest ------------------------------------------------------

manhattan_plot_line_label <-
  # Use the new data
  ggplot(plot_data) +
  geom_point(aes(x = x, y = p.value, color = color_group)) +
  geom_hline(
    yintercept = pv_threshold,
    colour = 'lightblue'
  ) +
  # Adding labels last draws them on top of threshold lines
  geom_text_repel(aes(x = x, y = p.value, label = label), box.padding = 0.1) +
  geom_vline(
    xintercept = -buffer/2,
    colour = 'lightblue',
    alpha = 0.5,
    linetype = 2
  ) +
  # Add every other one
  geom_vline(
    aes(xintercept = c(center + median(1:n_terms) + buffer/2)),
    colour = 'lightblue',
    alpha = 0.5,
    linetype = 2
  ) +
  scale_y_continuous(
    expression(paste(-log[10]('P-value'))),
    limits = c(0, NA),
    expand = expand_scale(mult = c(0, 0.05))) +
  # Use the x labels to mark the `response` each set of results corresponds to
  scale_x_continuous(
    breaks = model_set_spacing$center,
    labels = model_set_spacing$response
  ) +
  # Remove color legend
  scale_color_discrete(guide = FALSE) +
  thm

print(manhattan_plot_line_label)


