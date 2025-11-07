
library(tidyverse)

# Recreate your table
df <- tribble(
  ~CellLine, ~ABCB1, ~ABCG2, ~ABCB5, ~ABCA8, ~CFTR,
  "OVCAR8", 12.75593405, 9.411187159, 20.55694137, 13.7121573, 13.88854482,
  "TOV21G", 13.40913049, 14.54248001, 15.91602529, 14.73275617, 17.77961949,
  "OVCAR3", 15.05896991, 11.00495557, 21.29229169, 19.22865073, 15.09539169,
  "OV90",   12.12370436, 8.637280272, 12.20359926, 18.76012704, 7.213727261
)

# Convert to long format and compute log2RQ = -ΔCt
df_long <- df %>%
  pivot_longer(-CellLine, names_to = "Gene", values_to = "DeltaCt") %>%
  mutate(log2RQ = -DeltaCt)

# Plot: baseline expression across cell lines
ggplot(df_long, aes(x = Gene, y = log2RQ, fill = CellLine)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Baseline Expression of Genes Across Cell Lines",
    y = "log2 Relative Quantity (log2RQ = -ΔCt)",
    x = "Gene"
  )

ggplot(df_long, aes(x = Gene, y = log2RQ, fill = CellLine)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "PuOr") +   # Purple–Orange, but we can clip to the purple/gray range
  theme_minimal(base_size = 14) +
  labs(
    title = "Baseline Expression of Genes Across Cell Lines",
    y = "log2 Relative Quantity (log2RQ = -ΔCt)",
    x = "Gene"
  )
ggplot(df_long, aes(x = Gene, y = CellLine, fill = log2RQ)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low =  "#000000" , # Iowa yellow
    high =  "#FFCD00"  # black
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Baseline Expression Heatmap",
    x = "Gene Expression", y = "Cell Line", fill = "log2RQ"
  )


