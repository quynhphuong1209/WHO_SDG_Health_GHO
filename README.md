# WHO Health Dashboard

![Shiny](https://img.shields.io/badge/shiny-%232C3E50.svg?style=for-the-badge&logo=r&logoColor=white)
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)

An interactive dashboard for visualizing and analyzing WHO health indicators with R Shiny.

**Live App:** [https://quynhphuong.shinyapps.io/WHO_SDG_Health_GHO/](https://quynhphuong.shinyapps.io/WHO_SDG_Health_GHO/)

## Features

- Interactive visualization of health indicators across countries and regions
- Time series analysis from 1932 to 2022
- Regional comparison using box plots
- Correlation analysis between indicators
- Raw data exploration and export
- Responsive design for different screen sizes

## Indicators Included

1. **Maternal Mortality Rate** (per 100,000 live births)
2. **Under-5 Mortality Rate** (per 1,000 live births)
3. **Infectious Disease Prevalence** (per 100,000 population)
4. **Health Service Coverage** (% of population)

## How to Use

### Controls (Left Sidebar)
- **Select Indicator**: Choose from 4 health indicators
- **Year Range**: Adjust the time period (1932-2022)
- **Country/Region**: Select multiple countries for comparison

### Tabs
1. **Overview**: Summary statistics and comparison of all 4 indicators
2. **Trends**: Time series visualization
3. **Regional**: Box plots and statistics by region
4. **Correlation**: Heatmap of indicator correlations
5. **Raw Data**: Full dataset with export options

## Technical Details

### Built With
- [R Shiny](https://shiny.rstudio.com/)
- [shinydashboard](https://rstudio.github.io/shinydashboard/)
- [plotly](https://plotly.com/r/) for interactive visualizations
- [tidyverse](https://www.tidyverse.org/) for data manipulation
- [DT](https://rstudio.github.io/DT/) for interactive tables

### Data
- Sample data generated programmatically
- Includes 7 countries across 4 regions (Asia, Americas, Europe, Africa)
- Historical trends with realistic variations

## Installation (Local Development)

1. Clone this repository
2. Install required R packages:
```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "DT", "shinycssloaders"))
