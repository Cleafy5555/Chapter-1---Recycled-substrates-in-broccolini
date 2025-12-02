# ==========================================
# Global fixed-effects model + emmeans vs Control
# ==========================================
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(emmeans)
library(patchwork)
library(readr)
library(tidyr)
# ---- Paths ----
infile  <- "//ad.uws.edu.au/dfshare/HomesHWK$/90955975/Desktop/Claudia/PhD OG werk/GROWTH TRIAL/Dry_weight_POLYTUNNEL BROCS.xlsx"
sheet   <- "Whole plant"
out_dir <- "//ad.uws.edu.au/dfshare/HomesHWK$/90955975/Desktop/Claudia/PhD OG werk/GROWTH TRIAL"
dir.create(file.path(out_dir, "stats"), showWarnings = FALSE, recursive = TRUE)

# ---- Labels & colours ----
substrate_names <- c(
  "0"  = "Control",
  "1"  = "Recycled textile",
  "2"  = "Paper waste",
  "3"  = "Perlite",
  "4"  = "Biochar",
  "5"  = "Enriched Biochar",
  "6"  = "Coco coir",
  "7"  = "Sawdust",
  "8"  = "Peat",
  "9"  = "Mushroom waste",
  "10" = "Coffee grounds",
  "11" = "Sand vermicompost",
  "12" = "Peat2"
)
control_color <- "#7B7D7E"
treatment_colors <- c(
  "#C1DFF0", "#3A3D3E", "#e7298a", "#b2df8a", "#66a61e", "#FBEA4B",
  "#AF6E4D", "#5E4DB0", "#5EC97A", "#EC9CDE", "#fdbf6f", "#fb9a99"
)
palette_named <- c(
  setNames(control_color, "Control"),
  setNames(treatment_colors, substrate_names[names(substrate_names) != "0"])
)

# ---- Read & tidy ----
raw <- read_excel(infile, sheet = sheet)
dryw <- raw %>%
  transmute(
    Substrate   = as.character(Substrate),
    plant_part  = toupper(as.character(plant_part)),
    dweight     = as.numeric(dweight)
  ) %>%
  filter(is.finite(dweight)) %>%
  mutate(
    # accept "S0..S12" or "0..12"
    Substrate = ifelse(grepl("^S\\d+$", Substrate), sub("^S", "", Substrate), Substrate),
    plant_part = recode(plant_part, "R"="Root", "S"="Shoot", "L"="Leaf", .default = plant_part),
    SubstrateLabel = substrate_names[Substrate]
  ) %>%
  mutate(
    Substrate      = factor(Substrate, levels = names(substrate_names)),
    SubstrateLabel = factor(SubstrateLabel, levels = unname(substrate_names)),
    plant_part     = factor(plant_part, levels = c("Root","Shoot","Leaf"))
  )

# ---- Global fixed-effects model ----
model <- lm(dweight ~ Substrate * plant_part, data = dryw)
anova_tbl <- anova(model)
write.csv(as.data.frame(anova_tbl), file.path(out_dir, "stats", "anova_global_fixed.csv"))

# ---- EMMEANS contrasts vs control within each part, BH adjusted globally ----

emm <- emmeans::emmeans(model, specs = ~ Substrate | plant_part)

# Make sure the control level matches exactly
print(levels(dryw$Substrate))  # sanity check
ctrl_level <- which(levels(dryw$Substrate) == "0")  # index of Control

contr <- emmeans::contrast(
  emm,
  method = "trt.vs.ctrl",
  ref    = ctrl_level        # use index; avoids name mismatch
)

contr_df <- summary(contr, infer = c(TRUE, TRUE), adjust = "none") |>
  as.data.frame() |>
  dplyr::mutate(
    p_adj = p.adjust(p.value, method = "BH"),
    sig   = dplyr::case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01  ~ "**",
      p_adj < 0.05  ~ "*",
      TRUE          ~ "ns"
    ),
    # pull the substrate code (contrast is like "1 - 0")
    Substrate = sub(" - 0$", "", contrast),
    SubstrateLabel = substrate_names[Substrate]
  )



levels(dryw$Substrate)
str(emm)

#check what got starred
View(contr_df)




# --- Tidy the contrast names to codes & pretty labels ---
contr_tidy <- contr_df %>%
  mutate(
    Substrate = sub("^Substrate([0-9]+)\\s*-.*$", "\\1", contrast),      # "Substrate7 - Substrate0" -> "7"
    SubstrateLabel = substrate_names[Substrate]
  ) %>%
  select(plant_part, Substrate, SubstrateLabel, estimate, SE, df, t.ratio, p.value, p_adj, sig)

# (If you prefer BH within each plant part instead of global, swap this line in:)
# contr_tidy <- contr_df %>% group_by(plant_part) %>% mutate(p_adj = p.adjust(p.value, "BH")) %>% ungroup()

# --- Build per-panel asterisk annotations (close to top whisker) ---
make_ann <- function(part_name, df_part, contrast_df) {
  sig_df <- contrast_df %>%
    filter(plant_part == part_name, !is.na(p_adj), p_adj < 0.05) %>%
    select(SubstrateLabel, p_adj)
  
  if (!nrow(sig_df)) return(data.frame())
  
  ymax <- df_part %>%
    group_by(SubstrateLabel) %>%
    summarise(ymax = max(dweight, na.rm = TRUE), .groups = "drop")
  
  rng  <- range(df_part$dweight, na.rm = TRUE)
  bump <- 0.02 * (rng[2] - rng[1])  # small headroom
  
  sig_df %>%
    left_join(ymax, by = "SubstrateLabel") %>%
    mutate(
      label = case_when(p_adj < 0.001 ~ "***",
                        p_adj < 0.01  ~ "**",
                        TRUE          ~ "*"),
      y = ymax + bump
    ) %>%
    select(SubstrateLabel, y, label)
}

ann_root  <- make_ann("Root",  filter(dryw, plant_part=="Root"),  contr_tidy)
ann_shoot <- make_ann("Shoot", filter(dryw, plant_part=="Shoot"), contr_tidy)
ann_leaf  <- make_ann("Leaf",  filter(dryw, plant_part=="Leaf"),  contr_tidy)

# --- Plot helper (no grid, shared legend) ---
plot_part <- function(df_part, title_txt, ann_df) {
  p <- ggplot(df_part, aes(x = SubstrateLabel, y = dweight, fill = SubstrateLabel)) +
    geom_boxplot(width = 0.7, outlier.shape = 21, outlier.alpha = 0.8, size = 0.4) +
    geom_text(data = ann_df, aes(x = SubstrateLabel, y = y, label = label),
              inherit.aes = FALSE, color = "red", size = 7) +
    scale_fill_manual(values = palette_named) +
    labs(title = title_txt, x = "Substrate", y = "Dry weight (g)") +
    theme_classic(base_size = 12) +
    theme(axis.text.x  = element_text(angle = 30, hjust = 1, vjust = 1),
          plot.title   = element_text(hjust = 0, face = "bold"),
          legend.position = "right",
          legend.text     = element_text(size = 14),   # legend labels bigger
          legend.title    = element_text(size = 14),   # legend title bigger (if present)
          legend.key.size = unit(1.2, "cm"))            # box size)
  if (nrow(ann_df)) p <- p + expand_limits(y = max(ann_df$y, na.rm = TRUE) * 1.02)
  p
}

p_root  <- plot_part(filter(dryw, plant_part=="Root"),  "Root",  ann_root)
p_shoot <- plot_part(filter(dryw, plant_part=="Shoot"), "Shoot", ann_shoot)
p_leaf  <- plot_part(filter(dryw, plant_part=="Leaf"),  "Leaf",  ann_leaf)

combined <- (p_root / p_shoot / p_leaf) +
  plot_layout(ncol = 1, guides = "collect") & theme(legend.position = "right")

# Save
ggsave(file.path(out_dir, "boxplots_globalANOVA_emmeans_stars.png"),
       combined, width = 10, height = 14, dpi = 300)
