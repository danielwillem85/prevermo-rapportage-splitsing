# CSV Data Extractor

This R Shiny app lets a user upload multiple CSV files and creates frequency tables from the extracted data in each CSV. Each file's results start hidden behind a `Show rows` toggle. A `Percentage` dropdown scales the displayed frequencies.

For each CSV, the app:

1. Finds a cell containing `Date of Birth`, ignoring letter case.
2. Uses the cell immediately to its right as the top-left corner of the extraction area.
3. Finds a cell containing `Tags`, ignoring letter case.
4. Scans right from `Tags` until it finds a non-empty cell.
5. Uses the cell one row down and one column left of that non-empty cell as the top-right corner.
6. Extracts rows downward until the first completely blank row in the selected columns.
7. Uses the first extracted row as column names.
8. Creates one frequency table per extracted column and displays them 3 per row.
9. Multiplies each frequency by the selected percentage and divides by 100.
10. Colors adjusted frequency cells red for values 10 or below, yellow for values below 20 and above 10, and green otherwise.

## Run

From this folder:

```r
shiny::runApp("app.R")
```

If `shiny` is not installed:

```r
install.packages("shiny")
```
