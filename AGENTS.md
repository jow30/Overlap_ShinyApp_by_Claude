# Project: Gene Set Venn Shiny App

## Purpose
Build a Shiny app that visualizes gene set overlaps using Venn diagrams and upset plots.

## Requirements

### Input

Accepts gene lists in the following formats:
- a folder of txt or csv files where each line in the first column is a gene
- a excel file where each sheet is a gene list
- a rds file that stores vectors of genes in a list

After loading, check the first line to see whether it is a header or not. If it is a header, give user a warning and remove it. If it is not a header, keep it. A header might be something like NA, "gene", "gene name", "gene id", "gene symbol", "identifier", "gene list", "ID". Also check whether there are duplicate genes in the input files. If there are, give user a warning and keep only the first occurrence. 

### Preview and selection

Display the loaded gene lists in a table where user can use check boxes to select for specific gene lists to plot. The name of the gene list should be the name of the file (without the extension) or the name of the sheet (for excel files) or the name of the list (for rds files). If there are duplicate names, append a number to the name to make it unique.

### Plotting both Venn diagram and upset plot

Allows interactive adjustment after plot generation, options should include figure width, height, titles, colors, transparency, label size, and other commonly applied options in the orignal R function that is used to generate the plot. Please use a clean background without any grid lines or axis.

The plot should be immediately reactive to any adjustments from the user, including changing options, adjusting with the sliders, changing in the checkboxes, etc.

When more than five gene lists are selected, only upset plot should be displayed. Otherwise, both Venn diagram and upset plot should be displayed.

### Download

Allows figure download in various formats, including pdf, png, and jpeg

Allows intersections download in various formats, including txt, csv, and excel
- When downloading in txt or csv format, save each intersection to a separate file. The name of the file should indicate the gene lists that are included in the intersection.
- When downloading in excel format, save each intersection to a separate sheet. In each sheet, the gene list is saved in the first column and the column header equal to the combination name.

### Search for genes in the intersections

Show a search bar that allows user to search for genes in the intersections. When user types a gene name, the app should display the intersections that contain the gene.

## UI design

- A narrow left panel (about 20% of the width) for gene list loading options
- A wide right panel (about 80% of the width) for the display of gene list preview and figures
- Use separate panels for gene list preview, venn plot and options, upset plot and options, and search bar

## Code structure

- Use a modular approach with separate files for different components
- Use a reactive programming model
- Use a clean and organized file structure

## Testing and debuging 

- create a conda environment with all the required packages and dependencies
- create a test dataset with various gene lists in multiple formats
- fix bugs if any error occurs during testing
