# ============================================================
# global.R - Global configuration and library loading
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(VennDiagram)
library(UpSetR)
library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(stringr)
library(grid)
library(grDevices)
library(colourpicker)

# Suppress VennDiagram log files
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

# Source module files
source("modules/data_loader.R")
source("modules/gene_preview.R")
source("modules/venn_plot.R")
source("modules/upset_plot.R")
source("modules/download_handler.R")
source("modules/gene_search.R")

# Define known header patterns (case-insensitive)
HEADER_PATTERNS <- c(
  "^na$", "^gene$", "^gene name$", "^gene_name$", "^genename$",
  "^gene id$", "^gene_id$", "^geneid$", "^gene symbol$",
  "^gene_symbol$", "^genesymbol$", "^identifier$",
  "^gene list$", "^gene_list$", "^genelist$", "^id$",
  "^symbol$", "^name$", "^entrez$", "^ensembl$", "^refseq$",
  "^uniprot$", "^protein$", "^probe$", "^probe_id$"
)

# Maximum default palette colors (Tableau 10)
DEFAULT_COLORS <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
)

# Named color palettes for user selection
COLOR_PALETTES <- list(
  "Tableau 10" = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
                   "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"),
  "Pastel Rainbow" = c("#A8D8EA", "#F9B5AC", "#B5EAD7", "#FFD6A5", "#C9B1FF",
                       "#FDFFB6", "#FFC6FF", "#BDB2FF", "#CAFFBF", "#FFADAD"),
  "Ocean Breeze" = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51",
                     "#606C38", "#283618", "#DDA15E", "#BC6C25", "#FEFAE0"),
  "Sunset Warm"  = c("#FF6B6B", "#FFA36C", "#FFD93D", "#6BCB77", "#4D96FF",
                     "#FF78C4", "#A66CFF", "#45CFDD", "#F94C66", "#FF9E9E"),
  "Forest Earth" = c("#2D6A4F", "#52B788", "#95D5B2", "#B7E4C7", "#D8F3DC",
                     "#774936", "#A3805A", "#C4A77D", "#E6CCB2", "#DDB892"),
  "Vivid Bold"   = c("#E63946", "#457B9D", "#2A9D8F", "#E9C46A", "#264653",
                     "#F72585", "#7209B7", "#3A0CA3", "#4361EE", "#4CC9F0")
)
