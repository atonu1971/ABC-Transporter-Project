#### -------------------------------------------------------------
#### 1. Load Required Libraries
#### -------------------------------------------------------------
library(tidyverse)
library(RColorBrewer)

#### -------------------------------------------------------------
#### 2. Set Working Directory to Your CSV Folder
#### -------------------------------------------------------------
setwd("/Users/atonuchakra/Downloads/ABC_PubMed") 
# Change only if your folder changes

#### -------------------------------------------------------------
#### 3. Load ALL CSVs Automatically
#### -------------------------------------------------------------
files <- list.files(pattern = "*.csv")

# Clean gene/family label from filename
label_from_filename <- function(filename) {
  filename %>%
    gsub("_PubMed.*", "", .) %>%
    gsub("_Timeline.*", "", .) %>%
    gsub("\\(.*\\)", "", .) %>%
    gsub("_", "", .) %>%
    trimws()
}

# Read CSVs, skipping the first line (search query)
df_all <- map_dfr(files, function(f) {
  df <- read_csv(f, skip = 1, show_col_types = FALSE)
  df$Gene <- label_from_filename(f)
  df
})

#### -------------------------------------------------------------
#### 4. Inspect: Check which years go up to 2025/2026
#### -------------------------------------------------------------
df_all %>% group_by(Gene) %>% summarise(max_year = max(Year))

#### -------------------------------------------------------------
#### 5. Build a High-Quality Color Palette
#### -------------------------------------------------------------
genes <- sort(unique(df_all$Gene))
n_genes <- length(genes)

# Use Set3 (supports up to 12 colors) and extend if needed
my_cols <- colorRampPalette(brewer.pal(12, "Set3"))(n_genes)
names(my_cols) <- genes

# OPTIONAL: make ABCB1 bold red or black
my_cols["ABCB1"] <- "#E41A1C"   # bright red
# my_cols["ABCB1"] <- "#000000" # or black

#### -------------------------------------------------------------
#### 6. Plot ALL ABC Transporter Publication Trends
#### -------------------------------------------------------------
ggplot(df_all, aes(x = Year, y = Count, color = Gene)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "PubMed Publications Over Time (ABC Transporter Families)",
    x = "Year",
    y = "Number of Publications"
  ) +
  scale_color_manual(values = my_cols) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18)
  )
