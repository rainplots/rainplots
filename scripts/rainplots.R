# Library -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggdendro)
library(gridExtra)
library(ggplot2)

# Import ------------------------------------------------------------------

plot_data <-
  readRDS('data/tutorial_plot_data.rds')


# P-Value transformation --------------------------------------------------

plot_data <-
  plot_data %>%
  mutate(p.value = -1 * log10(p.value))



# Theme + Palette ---------------------------------------------------------


## Palette

palette <-
  # Blue
  c("#053061",
    "#313695",
    "#4575b4",
    "#74add1",
    "#abd9e9",
    "#e0f3f8",
    "#fee090",
    "#fdae61",
    "#f46d43",
    "#d73027",
    "#a50026",
    '#67001f')
# Red

# Calculate symmetric limits based on most extreme value
max_abs_estimate <- max(abs(plot_data$estimate))

max_lim <- max_abs_estimate
min_lim = -1 * max_lim

## theme

thm <-
  # Good starting theme + set text size
  theme_light(base_size = 18) +
  theme(
    # Remove axis ticks and titles
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),

    # Remove gridlines and boxes
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    legend.key = element_blank(),

    # White backgrounds
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white'),
    legend.background = element_rect(fill = 'white'),

    # Angle text
    axis.text.x.top  = element_text(angle = 45, hjust = 0)
  )

# Bare-bones rainplot -----------------------------------------------------

rainplot <-
  ggplot(plot_data) +
  geom_point(aes(x = response, y = term, colour = estimate, size = p.value))

print(rainplot)

# Basic Rainplot ----------------------------------------------------------

rainplot <-
  ggplot(plot_data) +
  geom_point(aes(x = response, y = term, colour = estimate, size = p.value))  +
  scale_x_discrete(position = 'top') +
  scale_size_area(expression(paste(-log[10]('P-value'))), max_size = 12) +
  scale_color_gradientn(
    'Effect Size Estimate',
    colors = palette,
    limits = c(min_lim, max_lim),
    breaks = c(min_lim,  min_lim / 2, 0 , max_lim/2, max_lim)
  ) +
  thm

print(rainplot)



# Outlines ----------------------------------------------------------------

rainplot <-
  ggplot(plot_data) +
  geom_point(aes(x = response, y = term, fill = estimate, size = p.value),
             shape = 21) +
  scale_x_discrete(position = 'top') +
  scale_size_area(expression(paste(-log[10]('P-value'))), max_size = 12) +
  scale_fill_gradientn(
    'Effect Size Estimate',
    colors = palette,
    limits = c(min_lim, max_lim),
    breaks = c(min_lim, min_lim / 2, 0 , max_lim / 2, max_lim)
  ) +
  thm

print(rainplot)


# P-value Thresholding ----------------------------------------------------


plot_data_thresholded <-
  plot_data %>%
  mutate(p.value = ifelse(p.value > 15, 15, p.value))

rainplot <-
  # Use the thresholded data
  ggplot(plot_data_thresholded) +
  geom_point(aes(x = response, y = term, color = estimate, size = p.value)) +
  scale_x_discrete(position = 'top') +
  scale_size_area(
    expression(paste(-log[10]('P-value'))),
    max_size = 12,
    breaks = c(5, 10, 15),
    labels = c('5', '10', '>=15')) +
  scale_color_gradientn(
    'Effect Size Estimate',
    colors = palette,
    limits = c(min_lim, max_lim),
    breaks = c(min_lim, min_lim / 2, 0 , max_lim / 2, max_lim)
  ) +
  thm


print(rainplot)

# Ordering by P-Value -----------------------------------------------------

# Calculate mean P-value for each metabolite
mpv <-
  plot_data %>%
  group_by(term) %>%
  summarise(mean_pv = mean(p.value))

# Order metabolites by average p-value
term_order <-
  mpv %>%
  arrange(mean_pv) %>%
  pull(term)

# Convert term to a factor, ordered by `term_order`
plot_data_pvo <-
  plot_data %>%
  mutate(term = factor(term, levels = term_order))


rainplot <-
  # Use the data with the term column ordered by mean P-value
  ggplot(plot_data_pvo) +
  geom_point(aes(x = response, y = term, colour = estimate, size = p.value)) +
  scale_x_discrete(position = 'top') +
  scale_size_area(expression(paste(-log[10]('P-value'))), max_size = 12) +
  scale_color_gradientn(
    'Effect Size Estimate',
    colors = palette,
    limits = c(min_lim, max_lim),
    breaks = c(min_lim, min_lim / 2, 0 , max_lim / 2, max_lim)
  ) +
  thm

print(rainplot)


# Ordering by Cluster -----------------------------------------------------

# Convert to matrix and reshape for clustering.
cluster_data <-
  plot_data %>%
  select(response, term, estimate) %>%
  spread(response, estimate)

rnms <-
  cluster_data$term

cluster_data <-
  cluster_data %>%
  select(-term) %>%
  as.matrix()

rownames(cluster_data) <- rnms

# cluster dependent variable terms
clust <- hclust(dist(cluster_data), method = 'ward.D2')

# `clust$order` orders `term` into clusters
term_order <-
  clust$labels[clust$order]

# Convert term to a factor, ordered by `term_order`
plot_data_clo <-
  plot_data %>%
  mutate(term = factor(term, levels = term_order))


rainplot <-
  # Use cluter ordered data
  ggplot(plot_data_clo) +
  geom_point(aes(x = response, y = term, colour = estimate, size = p.value)) +
  scale_x_discrete(position = 'top') +
  scale_size_area(expression(paste(-log[10]('P-value'))), max_size = 12) +
  scale_color_gradientn(
    'Effect Size Estimate',
    colors = palette,
    limits = c(min_lim, max_lim),
    breaks = c(min_lim, min_lim / 2, 0 , max_lim / 2, max_lim)
  ) +
  thm

print(rainplot)


# Adding dendrograms ------------------------------------------------------

dendro_dat <- segment(dendro_data(clust))

# Taking only the column with the longest label maintains alignment while
# simplifying plot layout
x_labels <-
  plot_data_clo$response %>%
  unique()

longest_x_label <-
  x_labels[[which.max(nchar(x_labels))]]

longest_x_label_data <-
  plot_data %>%
  filter(response == longest_x_label)

# Align the top of the dendrogram with the x-axis label
max_dendro <-
  max(abs(c(dendro_dat$y, dendro_dat$yend)))

offset <- max_dendro + 1

dendro <-
  # Empty ggplot with same layout and scale as rainplot
  ggplot() +
  geom_blank(aes(x = response, y = term), data = longest_x_label_data) +
  thm +
  # 'expand' controls whitespace around the dendrogram. The non-zero argument
  # may need to be increasesed if the line thickness of the dendrogram is
  # increased to make sure the entire dendrogram is plotted
  scale_x_discrete(position = 'top', expand = c(0, 0.03, 0, 0)) +
  # Draw dendrogram
  geom_segment(aes(x = (-y + offset), y = x, xend = (-yend + offset), yend=xend),
               colour = 'black',
               data = dendro_dat) +
  # Mave invisble unnessecary plot layout.
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = 'white'),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = 'white')
  )


grid.arrange(dendro, rainplot, ncol = 2, widths = c(3, 15))


