# Library -----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggdendro)
library(egg)
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

# Bare-bones heatmap -----------------------------------------------------

heatmap <-
  ggplot(plot_data) +
  geom_tile(aes(x = response, y = term, fill = p.value))

print(heatmap)

# Basic P-Value Heatmap ---------------------------------------------------


pv_heatmap <-
  ggplot(plot_data) +
  geom_tile(aes(x = response, y = term, fill = p.value)) +
  scale_x_discrete(position = 'top') +
  scale_fill_viridis_c(expression(paste(-log[10]('P-value')))) +
  thm

print(pv_heatmap)


# Basic Effect Size Estimate Heatmap --------------------------------------

es_heatmap <-
  ggplot(plot_data) +
  geom_tile(aes(x = response, y = term, fill = estimate)) +

  scale_x_discrete(position = 'top') +
  scale_fill_gradientn(
    'Effect Size Estimate',
    colors = palette,
    limits = c(min_lim, max_lim),
    breaks = c(min_lim,  min_lim / 2, 0 , max_lim/2, max_lim)
  ) +
  thm

print(es_heatmap)



# P-value Thresholding ----------------------------------------------------


plot_data_thresholded <-
  plot_data %>%
  mutate(p.value = ifelse(p.value > 15, 15, p.value))

heatmap_thresh <-
  # Use the thresholded data
  ggplot(plot_data_thresholded) +
  geom_tile(aes(x = response, y = term, fill = p.value)) +
  scale_x_discrete(position = 'top') +
  # Set the legend breaks and labels to account for the thresholding
  scale_fill_viridis_c(
    expression(paste(-log[10]('P-value'))),
    breaks = c(0, 5, 10, 15),
    labels = c('0', '5', '10', '>=15')
  ) +
  thm


heatmap_thresh

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

heatmap_base <-
  # Use the data with the term column ordered by mean P-value
  ggplot(plot_data_pvo) +
  scale_x_discrete(position = 'top') +
  thm

pv_heatmap_pvo <-
  heatmap_base +
  geom_tile(aes(x = response, y = term, fill = p.value)) +
  scale_fill_viridis_c(
    expression(paste(-log[10]('P-value')))
  )


pv_heatmap_pvo

es_heatmap_pvo <-
  heatmap_base +
  geom_tile(aes(x = response, y = term, fill = estimate)) +
  scale_fill_gradientn(
    'Effect Size Estimate',
    colors = palette,
    limits = c(min_lim, max_lim),
    breaks = c(min_lim,  min_lim / 2, 0 , max_lim/2, max_lim)
  )

es_heatmap_pvo


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


heatmap_base <-
  # Use cluter ordered data
  ggplot(plot_data_clo) +
  scale_x_discrete(position = 'top') +
  thm

pv_heatmap_clo <-
  heatmap_base +
  geom_tile(aes(x = response, y = term, fill = p.value)) +
  scale_fill_viridis_c(
    expression(paste(-log[10]('P-value')))
  )


pv_heatmap_clo

es_heatmap_clo <-
  heatmap_base +
  geom_tile(aes(x = response, y = term, fill = estimate)) +
  scale_fill_gradientn(
    'Effect Size Estimate',
    colors = palette,
    limits = c(min_lim, max_lim),
    breaks = c(min_lim,  min_lim / 2, 0 , max_lim/2, max_lim)
  )

es_heatmap_clo



# Adding dendrograms ------------------------------------------------------

dendro_dat <- segment(dendro_data(clust))


dendro <-
  # Empty ggplot with same layout and y-scale as heatmap
  ggplot() +
  geom_blank(aes(y = term), data = plot_data) +
  theme_dendro() +
  # 'expand' controls whitespace around the dendrogram. The non-zero argument
  # may need to be increasesed if the line thickness of the dendrogram is
  # increased to make sure the entire dendrogram is plotted
  scale_x_discrete(position = 'top', expand = c(0, 0.03, 0, 0)) +
  # Draw dendrogram
  geom_segment(aes(x = -y, y = x, xend = -yend, yend = xend),
               colour = 'black',
               data = dendro_dat)


ggarrange(dendro, es_heatmap_clo, ncol = 2, widths = c(3, 15))


# Side-by-Side Heatmaps ---------------------------------------------------


# 90 degree text and move legend below plot
sbs_heatmap_thm <-
  thm +
  theme(axis.text.x.top  = element_text(angle = 90),
        legend.position = 'bottom')

# Put legend title on top of bar
gcb <-  guides(fill = guide_colorbar(title.position = 'top', barwidth = 15))

pv_heatmap_clo <-
  pv_heatmap_clo +
  sbs_heatmap_thm +
  gcb +
  # Can only use regular text for legend title
  scale_fill_viridis_c('Negative Log10 P-value')

es_heatmap_clo <-
  es_heatmap_clo +
  sbs_heatmap_thm +
  gcb

ggarrange(pv_heatmap_clo, es_heatmap_clo, ncol = 2)


# One Set of Y-Axis Labels for Side-by-Side Heatmaps ----------------------


es_heatmap_clo_nl <-
  es_heatmap_clo +
  theme(axis.text.y = element_blank())


ggarrange(pv_heatmap_clo, es_heatmap_clo_nl, ncol = 2)
# Dendrogram and Side-by-Side Heatmaps ------------------------------------

ggarrange(dendro, pv_heatmap_clo, es_heatmap_clo_nl,
             ncol = 3,
             widths = c(1, 2, 2))



