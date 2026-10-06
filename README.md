# Video Game Analytics Dashboard

A one-page dark R Shiny dashboard for uploading, filtering and analyzing video-game sales data. The original R batch analysis is preserved.

## Features

- Upload CSV data and safely clean numeric sales/year values.
- Use optional standard columns; only a usable sales field is required.
- Dynamically filter All Games, Pokemon, Cricket, Football / Soccer, Racing, or Action / Adventure.
- Adjust Top N to 5, 10, 15, 20, 25, 50, 100 or a custom value from 1 to 1000.
- View filtered summary cards, key insights, eight charts, ranked tables, regional/year analysis and a data preview.
- Download the currently filtered dataset and current Top-N analysis results.
- Single scrollable page with dark styling and section navigation.

Category filters search game name, platform and publisher text (Action / Adventure uses Genre). Category and Top-N changes recalculate dependent results automatically. If fewer rows exist than the requested N, all available rows are shown.

## Dataset

Upload a CSV with at least one usable sales field: Global_Sales, Sales/Total_Sales/Worldwide_Sales, or any regional sales field (NA_Sales, EU_Sales, JP_Sales, Other_Sales). Standard headings are matched regardless of case or punctuation. Other standard columns are optional; missing dimensions get safe defaults or show unavailable results. If Global_Sales is absent, it is derived from available regional data. The bundled sample is data/raw/vgsales.csv; sales are treated as millions.

## Install packages

Run once in VS Code PowerShell:

```powershell
Rscript -e "install.packages(c('shiny','readr','dplyr','tidyr','ggplot2','DT'), lib=Sys.getenv('R_LIBS_USER'), repos='https://cloud.r-project.org')"
```

The app checks packages at startup and does not install them automatically.

## Run the Shiny dashboard

Open this project folder in VS Code and run:

```powershell
Rscript -e "shiny::runApp()"
```

Open the local URL printed in the terminal. Upload the dataset, click **Analyze Dataset**, choose a category and Top N value, then explore the sections. Use **Reset Filters** to restore All Games and Top 10.

## Run the original batch analysis

```powershell
Rscript main.R
```

The existing modular scripts in R/01 through R/10 remain part of the project and save cleaned data, tables, charts and the EDA report under data/processed and output/.
