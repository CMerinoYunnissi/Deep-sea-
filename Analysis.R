

####QUALITATIVE PLOT####

library(dplyr)
library(ggplot2)
library(extrafont)
library(cowplot)
library(patchwork)

setwd("/Users/catalinamerino/RP2")
list.files()

data2 <- read.csv(file = "JC257_Whale_Wood_experiment_data.csv", header = TRUE)





# Whole dataset plots 
phyla_colors <- c(
  "Annelida" = "#2ca02c",        # Green (classic for worms)
  "Arthropoda" = "#ff7f0e",     # Orange (traditional for arthropods)
  "Bryozoa" = "#d62728",        # Red (stands out for colonial organisms)
  "Chaetognatha" = "#8c564b",   # Brown (for arrow worms)
  "Cnidaria" = "#e377c2",       # Pink (traditional for cnidarians)
  "Echinodermata" = "#9467bd",  # Purple (standard for echinoderms)
  "Kinorhyncha" = "#17becf",    # Cyan (for kinorhynchs)
  "Metazoa indet" = "#7f7f7f",  # Gray (for unidentified)
  "Mollusca" = "#bcbd22",       # Olive (common for mollusks)
  "Nematoda" = "#1f77b4",       # Blue (classic for nematodes)
  "Nemertea" = "#393b79"        # Dark blue (for ribbon worms)
)


plot_1 <- data2 %>%
  mutate(X._indiv = suppressWarnings(as.numeric(X._indiv))) %>%
  filter(!is.na(Phylum), !is.na(X._indiv), X._indiv > 0) %>%
  group_by(Phylum) %>%
  summarise(total = sum(X._indiv, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  mutate(Phylum = factor(Phylum, levels = Phylum),
         percentage = total / sum(total) * 100) %>%
  
  ggplot(aes(x = Phylum, y = total, fill = Phylum)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%d (%.1f%%)", total, percentage)),
    vjust = -0.3, size = 3, family = "Calibri"  # Set font family here
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = phyla_colors) +
  labs(y = "No. of individuals", x = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "Calibri", size = 11),  # Global font settings
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5)
  )
plot_1






# Arthropoda plots (no legend)
arthro_data <- data2 %>%
  filter(Phylum == "Arthropoda") %>%
  mutate(X._indiv = suppressWarnings(as.numeric(X._indiv))) %>%
  filter(!is.na(X._indiv), X._indiv > 0)

malacostraca_data <- arthro_data %>% filter(Class == "Malacostraca")
other_classes_data <- arthro_data %>% filter(Class != "Malacostraca")

malacostraca_order <- malacostraca_data %>%
  group_by(Order) %>%
  summarise(total = sum(X._indiv, na.rm = TRUE), .groups = "drop")

other_classes <- other_classes_data %>%
  group_by(Class) %>%
  summarise(total = sum(X._indiv, na.rm = TRUE), .groups = "drop")

combined_arthro <- bind_rows(
  malacostraca_order %>% rename(group = Order),
  other_classes %>% rename(group = Class)
) %>%
  arrange(desc(total)) %>%
  mutate(group = factor(group, levels = group))
# Define orange tones (light to dark)
arthropoda_colors <- c(
  "#FFD79B",  # Light orange
  "#FFB347",  # Medium orange
  "#FF8C42",  # Orange
  "#E67E22",  # Dark orange (classic Arthropoda color)
  "#D35400"   # Deep orange
)



plot_2 <- combined_arthro %>%
  mutate(percentage = total / sum(total) * 100) %>%
  ggplot(aes(x = group, y = total, fill = group)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%d (%.1f%%)", total, percentage)),
    vjust = -0.3, size = 3, family = "Calibri"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = colorRampPalette(arthropoda_colors)(nrow(combined_arthro))) +  # Dynamic orange palette
  labs(y = "No. of individuals", x = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "Calibri", size = 11),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45, hjust = 1, size = 11, face = "italic"
    ),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
  )
plot_2







# Polychaete plots (no legend)
polychaete_data <- data2 %>%
  filter(Class == "Polychaeta") %>%
  mutate(X._indiv = suppressWarnings(as.numeric(X._indiv))) %>%
  filter(!is.na(Family), !is.na(X._indiv), X._indiv > 0)

summary_poly <- polychaete_data %>%
  mutate(Family = ifelse(is.na(Family) | Family == "", "Polychaeta indet.", Family)) %>%  # rename empty
  group_by(Family) %>%
  summarise(total = sum(X._indiv, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  mutate(Family = factor(Family, levels = Family))


summary_poly_clean <- summary_poly %>%
  mutate(
    # Group rare families (n = 1) into "Other (n≤1)"
    Family = ifelse(total <= 1, "Other", as.character(Family)),
    # Define factor levels (ordered by total, then place "Other (n≤1)" last)
    Family = factor(
      Family,
      levels = c(
        setdiff(unique(Family[order(-total)]), "Other"),  # Dominant families first
        "Other"  # Rare families last
      )
    )
  ) %>%
  group_by(Family) %>%
  summarise(total = sum(total), .groups = "drop") %>%
  arrange(desc(total))  # Final sort

poly_colors <- c(
  "#1B4332", "#2D6A4F", "#40916C",  # Dark greens for top 3 families
  "#52B788", "#74C476", "#95D5B2",  # Medium greens
  "#B7E4C7", "#D8F3DC", "#E5F5E0",  # Light greens
  "#F6E8C3",                        # Beige for "Other"
  "gray80"                           # Gray for rare families
)

plot_3 <- summary_poly_clean %>%
  mutate(percentage = total / sum(total) * 100) %>%
  ggplot(aes(x = Family, y = total, fill = Family)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = ifelse(total > 1, sprintf("%d (%.1f%%)", total, percentage), "")),  # Label only major groups
    vjust = -0.3, size = 3, family = "Calibri"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = poly_colors) +
  labs(y = "No. of individuals",  x = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "Calibri", size = 11),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1, 
      vjust = 1,                # Move labels closer to axis
      margin = margin(t = -2),  # Negative top margin pulls labels down
      face = "italic", 
      size = 10
    ),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5)
  )
plot_3

# Add titles to each plot
plot_1 <- plot_1 + 
  ggtitle("A Phylum") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 11))

plot_2 <- plot_2 + 
  ggtitle("B Arthropoda") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 11))

plot_3 <- plot_3 + 
  ggtitle("C Polychaeta") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 11))


combined_plot <- (plot_1 / plot_2 / plot_3) + 
  plot_layout(heights = c(1.2, 1, 1.5))  # Adjust relative heights

combined_plot

library(showtext)

# Register Calibri font family
font_add(
  family = "Calibri",
  regular = "/Library/Fonts/Calibri.ttf",
  bold    = "/Library/Fonts/Calibri Bold.ttf",
  italic  = "/Library/Fonts/Calibri Italic.ttf",
  bolditalic = "/Library/Fonts/Calibri Bold Italic.ttf"
)

# Enable showtext
showtext_auto()

ggsave(
  filename = "Figure_5.pdf",
  plot = combined_plot,
  device = "pdf",   # no Cairo needed
  width = 7, height = 12, units = "in"
)








library(ggplot2)
library(dplyr)

setwd("/Users/catalinamerino/RP2")
list.files()

#QUANTITATIVE

#import downloaded 
data <- read.csv(file = "sediment ID_CMY.csv", header = TRUE)

colnames(data)

data <- data %>%
  filter(Phylum != "Foraminifera") %>%
  mutate(Family..or.higher. = ifelse(
    Class.or.order == "Polychaeta" & 
      (is.na(Family..or.higher.) | trimws(Family..or.higher.) == ""),
    "Polychaeta indet.",
    Family..or.higher.
  ))




####Combined plot of proportion and density####

library(patchwork)
library(tidyverse)
library(scales)
library(patchwork)
library(viridis)
library(RColorBrewer)

# Data Preparation 
data_clean <- data %>%
  rename(
    abundance = `X._indiv`,
    taxon_rank = `Class.or.order`,
    family = `Family..or.higher.`
  ) %>%
  mutate(
    Distance = factor(Distance, levels = sort(unique(Distance))),
    taxon_rank = case_when(
      Phylum == "Arthropoda" & (is.na(taxon_rank) | taxon_rank == "") ~ "Arthropoda indet.",
      Phylum == "Arthropoda" & taxon_rank == "Crustacea" ~ "Other crustacea",
      TRUE ~ taxon_rank
    )
  )

# Filtered dataset (sensu stricto, no meiofauna)
data_sensustricto <- data_clean %>%
  filter(
    !Phylum %in% c("Kinorhyncha", "Nematoda"),
    !taxon_rank %in% c("Acari", "Copepoda", "Arachnida", "Ostracoda")
  )

# Theme Setup 
sci_theme <- theme_minimal(base_size = 10) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

# Color Palettes 
phyla_colors <- c(
  "Annelida" = "#2ca02c",
  "Arthropoda" = "#ff7f0e",
  "Bryozoa" = "#17becf",
  "Kinorhyncha" = "#d62728",
  "Metazoa indet" = "#7f7f7f",
  "Mollusca" = "#bcbd22",
  "Nematoda" = "#1f77b4"
)


arthropod_colors <- c(
  "Amphipoda" = "#FFD79B",          # Light orange
  "Ostracoda" = "#FFB347",          # Medium orange
  "Cumacea" = "#FF8C42",            # Strong orange
  "Copepoda" = "#FF6F31",           # Slightly more red-orange
  "Malacostraca" = "#C0392B",       # Dark red
  "Tanaidacea" = "#8E2A7A",         # Burgundy-purple
  "Acari" = "#E67E22",              # Bright orange
  "Arthropoda indet." = "#7f7f7f"   # Medium gray
)

polychaete_colors <- c(
  "#1B4332", "#40916C", "#52B788", "#95D5B2",
  "#B7E4C7", "#D8F3DC", "#2ca02c"
)



# Panel A & B: Overall Community
panel_A <- data_clean %>%
  filter(abundance > 0) %>%
  group_by(Distance, Phylum) %>%
  summarise(total = sum(abundance), .groups = "drop") %>%
  group_by(Distance) %>%
  mutate(proportion = total / sum(total)) %>%
  ggplot(aes(x = Distance, y = proportion, fill = Phylum)) +
  geom_col(position = "stack", width = 0.9, color = "black", size = 0.2) +  # thin, tightly packed, black outlines
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(values = phyla_colors) +
  labs(y = "Relative proportion", x = NULL) +
  sci_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )
panel_A 

# Panel B: Abundance plot
panel_B <- data_clean %>%
  filter(abundance > 0) %>%
  group_by(Distance, Phylum) %>%
  summarise(total = sum(abundance), .groups = "drop") %>%
  ggplot(aes(x = Distance, y = total, fill = Phylum)) +
  geom_col(position = "stack", width = 0.9, color = "black", size = 0.2) +
  scale_y_continuous(expand = c(0, 0.05)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(values = phyla_colors) +
  labs(x = NULL, y = "No. of individuals") +
  sci_theme +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )
panel_B

# Panel C: sensu stricto relative abundance
panel_C <- data_sensustricto %>%
  filter(abundance > 0) %>%
  group_by(Distance, Phylum) %>%
  summarise(total = sum(abundance), .groups = "drop") %>%
  group_by(Distance) %>%
  mutate(proportion = total / sum(total)) %>%
  ggplot(aes(x = Distance, y = proportion, fill = Phylum)) +
  geom_col(position = "stack", width = 0.9, color = "black", size = 0.2) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(values = phyla_colors) +
  labs(x = NULL, y = "Relative proportion") +
  sci_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )
panel_C

# Panel D: sensu stricto abundance
panel_D <- data_sensustricto %>%
  filter(abundance > 0) %>%
  group_by(Distance, Phylum) %>%
  summarise(total = sum(abundance), .groups = "drop") %>%
  ggplot(aes(x = Distance, y = total, fill = Phylum)) +
  geom_col(position = "stack", width = 0.9, color = "black", size = 0.2) +
  scale_y_continuous(expand = c(0, 0.05)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(values = phyla_colors) +
  labs(x = NULL, y = "No. of individuals") +
  sci_theme +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )
panel_D

# Panel E: Arthropods relative abundance
panel_E <- data_clean %>%
  filter(
    abundance > 0,
    Phylum == "Arthropoda",
    !is.na(taxon_rank)
  ) %>%
  group_by(Distance, taxon_rank) %>%
  summarise(total = sum(abundance), .groups = "drop") %>%
  group_by(Distance) %>%
  mutate(proportion = total / sum(total)) %>%
  ggplot(aes(x = Distance, y = proportion, fill = taxon_rank)) +
  geom_col(position = "stack", width = 0.9, color = "black", size = 0.2) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(values = arthropod_colors) +
  labs(x = NULL, y = "Relative proportion") +
  sci_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )
panel_E

# Panel F: Arthropods abundance
panel_F <- data_clean %>%
  filter(
    abundance > 0,
    Phylum == "Arthropoda",
    !is.na(taxon_rank)
  ) %>%
  group_by(Distance, taxon_rank) %>%
  summarise(total = sum(abundance), .groups = "drop") %>%
  ggplot(aes(x = Distance, y = total, fill = taxon_rank)) +
  geom_col(position = "stack", width = 0.9, color = "black", size = 0.2) +
  scale_y_continuous(expand = c(0, 0.05)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(values = arthropod_colors, guide = "none") +
  labs(x = NULL, y = "No. of individuals") +
  sci_theme +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )
panel_F

# Panel G: Polychaetes relative abundance
panel_G <- data_clean %>%
  filter(
    abundance > 0,
    Phylum == "Annelida",
    taxon_rank == "Polychaeta",
    !is.na(family)
  ) %>%
  group_by(Distance, family) %>%
  summarise(total = sum(abundance), .groups = "drop") %>%
  group_by(Distance) %>%
  mutate(proportion = total / sum(total)) %>%
  ggplot(aes(x = Distance, y = proportion, fill = family)) +
  geom_col(position = "stack", width = 0.9, color = "black", size = 0.2) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(values = polychaete_colors) +
  labs(x = "Distance", y = "Relative proportion") +
  sci_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )
panel_G

# Panel H: Polychaetes abundance
panel_H <- data_clean %>%
  filter(
    abundance > 0,
    Phylum == "Annelida",
    taxon_rank == "Polychaeta",
    !is.na(family)
  ) %>%
  group_by(Distance, family) %>%
  summarise(total = sum(abundance), .groups = "drop") %>%
  ggplot(aes(x = Distance, y = total, fill = family)) +
  geom_col(position = "stack", width = 0.9, color = "black", size = 0.2) +
  scale_y_continuous(expand = c(0, 0.05)) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  scale_fill_manual(values = polychaete_colors, guide = "none") +
  labs(x = "Distance", y = "No. of individuals") +
  sci_theme +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )
panel_H


# Combine Panels 

figure_6 <- (
  (panel_A + panel_B) /
    (panel_C + panel_D) /
    (panel_E + panel_F) /
    (panel_G + panel_H)
) +
  plot_layout(guides = "collect", tag_level = 'new') +
  plot_annotation(tag_levels = "A") & 
  theme(
    legend.position = "right",
    plot.tag = element_text(family = "Calibri", face = "bold", size = 11)
  )

figure_6






ggsave(
  filename = "Figure_6.pdf",
  plot = figure_6,
  device = "pdf",   # no Cairo needed
  width = 7, height = 10, units = "in"
)
























colnames(data_clean)
unique(data_clean$Sub.sample.number)

####NMDS PLOT####

comm_matrix <- data_clean %>%
  # Replace empty or NA Phylum with "Unknown"
  mutate(Phylum = ifelse(is.na(Phylum) | Phylum == "", "Unknown", Phylum)) %>%
  group_by(Sub.sample.number, Phylum) %>%
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Phylum,
    values_from = abundance,
    values_fill = 0
  )

comm_matrix <- comm_matrix %>%
  # Keep only rows where at least one phylum has abundance > 0
  filter(rowSums(select(., -Sub.sample.number)) > 0)

# 2️⃣ Prepare metadata
sample_info <- data_clean %>%
  select(Sub.sample.number, Distance) %>%
  distinct()


# 3️⃣ Run NMDS (Bray-Curtis distance)
set.seed(123)  # for reproducibility
nmds <- metaMDS(comm_matrix %>% select(-Sub.sample.number), 
                distance = "bray", k = 2, trymax = 100)

# Check stress
nmds$stress
# Compute stress
stress_value <- round(nmds$stress, 3)

# 4️⃣ Extract NMDS scores and join metadata

scores_df <- as.data.frame(scores(nmds, display = "sites"))
scores_df$Sub.sample.number <- comm_matrix$Sub.sample.number
scores_df <- dplyr::left_join(scores_df, sample_info, by = "Sub.sample.number")


# 5️⃣ Plot NMDS with ggplot2
nmds_plot <- ggplot(scores_df, aes(x = NMDS1, y = NMDS2, color = Distance)) +
  geom_point(size = 3) +
  theme_minimal()+
  labs(
    color = "Distance",
    title = paste0("NMDS Plot (Stress = ", stress_value, ")")
  ) +
  theme(plot.title = element_text(hjust = 0.5))

nmds_plot





# Export as high-quality PDF
ggsave("Figure_8.pdf", plot = nmds_plot, width = 8, height = 6, dpi = 300)







####PERMANOVA####


rm(comm_mat)
rm(comm_matrix)
library(dplyr)
library(vegan)


# Make sure Phylum is filled
comm_matrix <- data_clean %>%
  mutate(Phylum = ifelse(is.na(Phylum) | Phylum == "", "Unknown", Phylum)) %>%
  group_by(Sub.sample.number, Phylum) %>%
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Phylum, values_from = abundance, values_fill = 0)

# Keep only rows with at least one non-zero abundance
comm_matrix <- comm_matrix %>%
  filter(rowSums(select(., -Sub.sample.number)) > 0)

# Prepare metadata
sample_info <- data_clean %>%
  select(Sub.sample.number, Distance) %>%
  distinct()


# Ensure both are character
comm_matrix$Sub.sample.number <- as.character(comm_matrix$Sub.sample.number)
sample_info$Sub.sample.number <- as.character(sample_info$Sub.sample.number)

# Remove empty samples from sample_info
sample_info <- sample_info %>% filter(Sub.sample.number != "")

# Keep only common samples
common_samples <- intersect(comm_matrix$Sub.sample.number, sample_info$Sub.sample.number)

# Reorder comm_matrix and convert to data.frame
comm_mat <- comm_matrix %>%
  filter(Sub.sample.number %in% common_samples) %>%
  arrange(match(Sub.sample.number, common_samples)) %>%
  as.data.frame()

# Set row names
rownames(comm_mat) <- comm_mat$Sub.sample.number
comm_mat$Sub.sample.number <- NULL

# Reorder sample_info to match comm_mat
sample_info_ordered <- sample_info %>%
  filter(Sub.sample.number %in% common_samples) %>%
  arrange(match(Sub.sample.number, rownames(comm_mat)))

# Create group vector
group <- sample_info_ordered$Distance

# Verify alignment
all(rownames(comm_mat) == sample_info_ordered$Sub.sample.number)




# Calculate Bray-Curtis distance
bray_dist <- vegdist(comm_mat, method = "bray")

# Check dispersion (homogeneity of multivariate variance)
dispersion <- betadisper(bray_dist, group)
anova(dispersion)  # if p > 0.05, variances are homogeneous

# Run PERMANOVA
set.seed(123)
permanova_result <- adonis2(comm_mat ~ group, method = "bray", permutations = 999)

# View results
permanova_result






#### LOAD LIBRARIES ####
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(tibble) # for column_to_rownames()

#### 1️⃣ Build a single community matrix ####

comm_mat <- data_clean %>%
  # Replace missing/empty Phylum with "Unknown"
  mutate(Phylum = ifelse(is.na(Phylum) | Phylum == "", "Unknown", Phylum)) %>%
  group_by(Sub.sample.number, Phylum) %>%
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Phylum,
    values_from = abundance,
    values_fill = 0
  ) %>%
  filter(rowSums(select(., -Sub.sample.number)) > 0) %>%
  column_to_rownames("Sub.sample.number")

# Metadata
sample_info <- data_clean %>%
  select(Sub.sample.number, Distance) %>%
  distinct() %>%
  filter(Sub.sample.number %in% rownames(comm_mat)) %>%
  arrange(match(Sub.sample.number, rownames(comm_mat)))

group <- sample_info$Distance

# Verify alignment
stopifnot(all(rownames(comm_mat) == sample_info$Sub.sample.number))


#### 2️⃣ NMDS ####

set.seed(123) # reproducibility
nmds <- metaMDS(comm_mat, distance = "bray", k = 2, trymax = 100)

stress_value <- round(nmds$stress, 3)

scores_df <- as.data.frame(scores(nmds, display = "sites")) %>%
  tibble::rownames_to_column("Sub.sample.number") %>%
  left_join(sample_info, by = "Sub.sample.number")

# NMDS plot
nmds_plot <- ggplot(scores_df, aes(x = NMDS1, y = NMDS2, color = Distance)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    color = "Distance",
    title = paste0("NMDS Plot (Stress = ", stress_value, ")")
  ) +
  theme(plot.title = element_text(hjust = 0.5))

nmds_plot

ggsave("Figure_8.pdf", plot = nmds_plot, width = 8, height = 6, dpi = 300)


#### 3️⃣ PERMANOVA ####

# Bray-Curtis distance
bray_dist <- vegdist(comm_mat, method = "bray")

# Check homogeneity of multivariate dispersion
dispersion <- betadisper(bray_dist, group)
anova(dispersion)  # p > 0.05 means variances are homogeneous

# Run PERMANOVA
set.seed(123)
permanova_result <- adonis2(comm_mat ~ group, method = "bray", permutations = 999)
permanova_result



