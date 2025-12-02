
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(readxl)
library(ggpattern)
library(forcats)

#-----------------------------------------------------------------------
# 1. Load data
#-----------------------------------------------------------------------
broc_may <- read_excel(
  "\\\\ad.uws.edu.au\\dfshare\\HomesHWK$\\90955975\\Desktop\\Claudia\\PhD OG werk\\GROWTH TRIAL\\2024_May-Dec_Growth_Trial\\May-Dec 2024 Height data.xlsx"
)

# Expect columns at least:
# sample_number, treatment, infected, dweightshoot, dweightroot, dscore

broc_may <- broc_may %>%
  mutate(
    dweight_total = dweightshoot + dweightroot,
    treatment     = factor(treatment),
    infected      = factor(infected)  # 0/1 or no/yes, we'll standardise below
  )

# Standardise inoculation labels to "no"/"yes" -------------------------
broc_may <- broc_may %>%
  mutate(
    infected = case_when(
      infected %in% c(0, "0") ~ "no",
      infected %in% c(1, "1") ~ "yes",
      TRUE                    ~ as.character(infected)
    ),
    infected = factor(infected, levels = c("no", "yes"))
  )

# ------------------------------------------------------------------
# Colour palette (consistent across manuscript figures)
# ------------------------------------------------------------------
pal <- c(
  "control"      = "grey50",
  "biochar"      = "#b2df8a",
  "mushroom"     = "#88665D",
  "vermicompost" = "#F7E34C",
  "u_rockwools"  = "#EEC1AD",  # used rockwool (tomato)
  "u_rockwoolb"  = "#228224",  # used rockwool (cucumber)
  "n_rockwool"   = "#39B6E0"   # new rockwool
)

leg_labels <- c(
  "control"      = "control",
  "biochar"      = "biochar",
  "mushroom"     = "mushroom waste",
  "vermicompost" = "vermicompost",
  "u_rockwools"  = "used rockwool (tomato)",
  "u_rockwoolb"  = "used rockwool (cucumber)",
  "n_rockwool"   = "new rockwool"
)

# ensure the plotting order matches the palette
broc_may <- broc_may %>%
  mutate(treatment = factor(treatment, levels = names(pal)))

#-----------------------------------------------------------------------
# 2. EMMs for root & shoot dry weight (treatment × infected × tissue)
#-----------------------------------------------------------------------
long_dw <- broc_may %>%
  select(sample_number, treatment, infected, dweightshoot, dweightroot) %>%
  pivot_longer(
    c(dweightshoot, dweightroot),
    names_to  = "tissue",
    values_to = "dry_weight"
  ) %>%
  mutate(
    tissue = recode(tissue,
                    dweightshoot = "shoot",
                    dweightroot  = "root"),
    treatment = stats::relevel(treatment, ref = "control")
  ) %>%
  filter(!is.na(dry_weight))

# linear model (Gaussian is fine for EMMs)
m_dw   <- lm(dry_weight ~ treatment * tissue * infected, data = long_dw)
emm_dw <- emmeans(m_dw, ~ treatment * tissue * infected)

# contrasts: each treatment vs control within (tissue × infected)
ctr_dw <- emmeans(m_dw, ~ treatment | tissue * infected) %>%
  contrast(method = "trt.vs.ctrl", ref = "control") %>%
  as.data.frame() %>%
  mutate(
    treatment = sub(" - control", "", contrast),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )

emm_df_dw <- as.data.frame(emm_dw) %>%
  left_join(
    ctr_dw %>% select(treatment, tissue, infected, sig),
    by = c("treatment", "tissue", "infected")
  )

#-----------------------------------------------------------------------
# 3. EMMs for DSI (treatment × infected), treated as numeric
#-----------------------------------------------------------------------
m_dsi   <- lm(dscore ~ treatment * infected, data = broc_may)
emm_dsi <- emmeans(m_dsi, ~ treatment * infected)

ctr_dsi <- emmeans(m_dsi, ~ treatment | infected) %>%
  contrast(method = "trt.vs.ctrl", ref = "control") %>%
  as.data.frame() %>%
  mutate(
    treatment = sub(" - control", "", contrast),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )

emm_df_dsi <- as.data.frame(emm_dsi) %>%
  left_join(
    ctr_dsi %>% select(treatment, infected, sig),
    by = c("treatment", "infected")
  ) %>%
  mutate(tissue = "DSI")  # use 'tissue' as facet variable for convenience

#-----------------------------------------------------------------------
# 4. Combine dry-weight and DSI EMMs
#-----------------------------------------------------------------------
emm_all <- bind_rows(emm_df_dw, emm_df_dsi) %>%
  mutate(
    treatment = factor(treatment, levels = names(pal)),
    tissue    = factor(tissue, levels = c("root", "shoot", "DSI")),
    infected  = factor(infected, levels = c("no", "yes"))
  )

# headroom for stars, per facet
emm_all <- emm_all %>%
  group_by(tissue) %>%
  mutate(pad = 0.08 * diff(range(emmean, na.rm = TRUE))) %>%
  ungroup()

emm_all <- emm_all %>%
  mutate(
    lower.CL_plot = pmax(lower.CL, 0)   # cap at 0 for display
  )

#-----------------------------------------------------------------------
# 5. Plot: bars by substrate × inoculation, facets by tissue/root/DSI
#-----------------------------------------------------------------------


# position dodge for grouped bars
pd <- position_dodge(width = 0.8)

p_dw_dsi <- ggplot(
  emm_all,
  aes(
    x       = treatment,
    y       = emmean,
    fill    = treatment,
    pattern = infected,
    group   = interaction(treatment, infected)
  )
) +
  # Force root & shoot panels to extend to 60 on the y-axis
  geom_blank(
    data = subset(emm_all, tissue %in% c("root", "shoot")),
    aes(y = 60)
  ) +
  
  # Bars with solid black hatch for inoculated plants
  ggpattern::geom_col_pattern(
    position        = pd,
    width           = 0.8,
    colour          = "black",   # bar outline
    linewidth       = 0.3,
    pattern_colour  = "black",   # hatch outline
    pattern_fill    = "black",   # hatch fill -> solid black stripes
    pattern_size    = 0.2,       # thinner stripes (version-compatible)
    pattern_spacing = 0.015,
    pattern_density = 0.45
  ) +
  
  # Error bars (using truncated lower.CL_plot)
  geom_errorbar(
    aes(ymin = lower.CL_plot, ymax = upper.CL),
    position = pd,
    width    = 0.18,
    linewidth = 0.3
  ) +
  
  # Significance labels
  geom_text(
    aes(y = emmean + pad, label = sig),
    position = pd,
    size     = 4.5,
    fontface = "bold",
    colour   = "black"
  ) +
  
  facet_wrap(
    ~ tissue,
    nrow   = 1,
    scales = "free_y",
    labeller = as_labeller(
      c(root = "root",
        shoot = "shoot",
        DSI   = "DSI")
    )
  ) +
  
  scale_fill_manual(
    values = pal,
    labels = leg_labels,
    name   = "Substrate"
  ) +
  
  ggpattern::scale_pattern_manual(
    values = c("no" = "none", "yes" = "stripe"),
    name   = "Inoculated"
  ) +
  
  labs(
    x = "Substrate type",
    y = "Dry weight (g) / DSI"
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic(base_size = 13) +
  theme(
    panel.spacing.x  = unit(16, "pt"),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold"),
    legend.text      = element_text(size = 10.5),
    legend.title     = element_text(size = 12),
    plot.margin      = margin(8, 20, 8, 8)
  )

p_dw_dsi




# save – vector and raster ----------------------------------------------
ggsave(
  filename = "\\\\ad.uws.edu.au\\dfshare\\HomesHWK$\\90955975\\Desktop\\Claudia\\PhD OG werk\\THESIS\\Chapter 1\\Figures\\Fig1_DryWeight_DSI_bySubstrate.png",
  plot     = p_dw_dsi,
  width    = 11,
  height   = 4.5,
  dpi      = 300
)

