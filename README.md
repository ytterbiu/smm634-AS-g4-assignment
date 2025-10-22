# MSc AS - Term 1: SMM634 Analytics Methods for Business — Group Project

Term 1 group project for Analytics Methods for Business (50% of final grade).

## Structure

The directory structure is as follows

```{bash}
.
├── air.toml
├── automobile
│   ├── imports-85.data
│   ├── imports-85.names
│   ├── Index
│   └── misc
├── car_price.csv
├── CarAnalysisMPG.R
├── CarAnalysisSafetyPrice.R
├── CarDataMain.R
├── CarDataRead.R
├── CarSafetyPrice.R
├── docs
│   └── MSc_AS-SMM634-Group4-Project.html
├── Group assignment-20251021.Rproj
├── Group-Assignment.r
├── MSc_AS-SMM634-Group4-Project.html
├── MSc_AS-SMM634-Group4-Project.rmd
├── OnlineCarSafetyExplore.R
├── README.md
├── SMM634_Ass1.pdf
└── smm634_group_project.ipynb
```

### automobile

This folder contains data from https://archive.ics.uci.edu/dataset/10/automobile
which is were we (AS & BE) think the original data is from.

### docs

This folder contains the html file produced from the R Markdown file
`MSc_AS-SMM634-Group4-Project.rmd`. In the current workflow this needs to be
manually rendered and the html file committed and pushed.

### `CarDataMain.R`

Main working file for our analysis. Idea is to use a modular structure to make
collaboration easier.

## Contents

- `MSc_AS-SMM634-Group4-Project.rmd` — main analysis/report (source)
- `docs/` — rendered HTML for GitHub Pages (output)
- `car_price.csv` — input data file
- `figs/` — exported figures (when produced)

## Requirements

- R (≥ 4.x)
- Packages: at minimum **rmarkdown** (others as used in the Rmd)

For R Markdown install core package:

```r
install.packages("rmarkdown")
```
