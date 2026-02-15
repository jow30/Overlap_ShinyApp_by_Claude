# Gene Set Venn & UpSet Visualization Dashboard

A polished, interactive Shiny application for visualizing overlaps between multiple gene lists. This tool provides both high-quality Venn diagrams (for up to 5 sets) and UpSet plots (for any number of sets), along with searching and data export capabilities.

## 🚀 Features

-   **Multi-format Input**: Load gene lists from a directory (txt/csv), multi-sheet Excel files, or RDS files.
-   **Smart Preprocessing**: Automatic header detection and duplicate gene removal with user notifications.
-   **Interactive Visualization**:
    -   **Venn Diagrams**: Clean, publication-ready diagrams with adjustable transparency, labels, and margins.
    -   **UpSet Plots**: Handles complex overlaps for more than 5 gene lists with customizable ordering and bar colors.
-   **Global Search**: Instantly find which specific intersections a gene belongs to across all selected sets.
-   **Export Options**:
    -   **Figures**: Download plots in PDF, PNG, or JPEG formats.
    -   **Data**: Export intersection gene lists in Excel, CSV, or plain text formats.

## 🛠️ Installation

### 1. Prerequisites
Ensure you have [Conda](https://docs.conda.io/en/latest/) installed on your system.

### 2. Create the Environment
Clone this repository and create the required environment using the provided `environment.yml` file:

```bash
# Clone the repository
git clone https://github.com/jow30/Overlap_ShinyApp_by_Claude.git
cd Overlap_ShinyApp_by_Claude

# Create the conda environment
conda env create -f environment.yml

# Activate the environment
conda activate gene_venn_app
```

## 💻 How to Run

### Option A: From the Terminal
With the environment activated, run:
```bash
R -e "shiny::runApp('app')"
```

### Option B: From RStudio
1. Open any file in the `app/` directory (e.g., `ui.R` or `server.R`).
2. Click the **Run App** button in the top right of the editor.

## 📁 Usage

1.  **Upload Data**: Use the sidebar to choose your input type and upload your files.
2.  **Select Lists**: In the **Preview** tab, use the checkboxes to select which gene lists to compare.
3.  **Customize Plots**: Switch to the **Venn** or **UpSet** tabs to adjust visual parameters (colors, sizes, etc.).
4.  **Search**: Use the **Search** tab to find specific genes within the overlapping sets.
5.  **Download**: Click the download buttons provided in the plot or search tabs to save your results.

## 📄 License
This project is provided for research and visualization purposes.
