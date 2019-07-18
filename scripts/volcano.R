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
ese_threshold <- 0.25

plot_data <-
  plot_data %>%
  mutate(pv_extreme = p.value > pv_threshold,
         es_high = estimate > ese_threshold,
         es_low = estimate < -1 * ese_threshold) %>%
  mutate(
    extreme = case_when(
      pv_extreme & es_high ~ 'high',
      pv_extreme & es_low  ~ 'low',
      TRUE                 ~ NA_character_
    ))

plot_data <-
  plot_data %>%
  mutate(label = ifelse(is.na(extreme), NA, term))

# Extreme Count -----------------------------------------------------------

extreme_count <-
  plot_data %>%
  count(response, wt = !is.na(extreme))


extreme_count  <-
  extreme_count %>%
  mutate(x = -0.7, y = 50) %>%
  mutate(label = paste(n, 'Extreme Result(s)'))

# Theme + Limits ----------------------------------------------------------

thm <-
  # Good starting theme + set text size
  theme_light(base_size = 18) +
  theme(
    # Remove Grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    # White background
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white')
  )

max_abs_estimate <- max(abs(plot_data$estimate))

max_lim <- max_abs_estimate
min_lim = -1 * max_lim


# Barebones Volcano Plot --------------------------------------------------

volcano_plot <-
  ggplot(plot_data) +
  geom_point(aes(x = estimate, y = p.value))

print(volcano_plot)


# Volcano Plot ------------------------------------------------------------

volcano_plot <-
  volcano_plot +
  scale_x_continuous('Effect Size Estimate',
                     limits =  c(min_lim, max_lim),
                     breaks = seq(min_lim, max_lim, length.out = 5)) +
  scale_y_continuous(expression(paste(-log[10]('P-value'))))  +
  thm

print(volcano_plot)

# Facetted Volcano Plot ---------------------------------------------------

volcano_plot <-
  volcano_plot +
  scale_x_continuous('Effect Size Estimate',
                     limits =  c(min_lim, max_lim),
                     breaks = seq(min_lim, max_lim, length.out = 5)) +
  scale_y_continuous(expression(paste(-log[10]('P-value'))))  +
  thm +
  facet_wrap(~response)

# Point Size --------------------------------------------------------------

volcano_plot_size <-
  ggplot(plot_data) +
  geom_point(aes(x = estimate, y = p.value), size = 4) +
  scale_x_continuous('Effect Size Estimate',
                     limits =  c(min_lim, max_lim),
                     breaks = seq(min_lim, max_lim, length.out = 5))  +
  scale_y_continuous(expression(paste(-log[10]('P-value')))) +
  thm +
  facet_wrap(~response)

print(volcano_plot_size)


# Values of Interest ------------------------------------------------------
volcano_plot_line_color_label <-
  ggplot(plot_data) +
  # Color by more extreme values
  geom_point(aes(x = estimate, y = p.value, color = extreme)) +
  # Add P-value threshold line
  geom_hline(yintercept = pv_threshold, colour = 'lightblue') +
  # Add effect size estimate lines
  geom_vline(xintercept = ese_threshold, colour = 'lightblue') +
  geom_vline(xintercept = -1 * ese_threshold, colour = 'lightblue') +
  # Adding labels last draws them on top of threshold lines
  geom_label_repel(aes(x = estimate, y = p.value, fill = extreme, label = label)) +
  scale_x_continuous('Effect Size Estimate',
                     limits =  c(min_lim, max_lim),
                     breaks = seq(min_lim, max_lim, length.out = 5)) +
  scale_y_continuous(expression(paste(-log[10]('P-value')))) +
  # Remove color and fill legend
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(guide = FALSE) +
  thm +
  facet_wrap(~response)


volcano_plot_line_color_label


# Summary Statistics ------------------------------------------------------


volcano_plot_line_color_label_anno <-
  volcano_plot_line_color_label +
  geom_text(aes(x = x, y = y, label = label),
            hjust = 0,
            data = extreme_count)


print(volcano_plot_line_color_label_anno)
