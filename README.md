# Tennis Line Calling Analysis

**Fall 2024 · SKKU Data Analysis in Sport Science (SPT5062) Final Project**

## Overview

This repository archives the R scripts from my final-project submission for **Data Analysis in Sport Science (SPT5062)** at Sungkyunkwan University.

The project investigated whether the introduction of **automated line calling in Grand Slam tennis tournaments** affected match duration. The submitted analysis collected match information from Tennis24 and used a Difference-in-Differences / fixed-effects regression framework.

According to the final presentation, the project collected **3,878 Grand Slam matches** over the 2019–2023 study period and reported an estimated reduction of approximately **1.70 minutes in average set duration** for the analyzed automated-line-calling setting.

## Project Workflow

| Script | Purpose |
| --- | --- |
| `src/01_collect_match_urls.R` | Collect tournament match URLs with RSelenium |
| `src/02_collect_match_data.R` | Collect match-level information from the discovered URLs |
| `src/03_prepare_dataset.R` | Clean, transform, and merge the collected datasets |
| `src/04_analyze_match_duration.R` | Construct analysis variables and run the Difference-in-Differences / fixed-effects regression |

## Methods and Technologies

- R
- RSelenium
- rvest
- dplyr / tidyr / tidyverse
- `plm`
- `lmtest`
- `stargazer`
- Web data collection
- Difference-in-Differences
- Fixed-effects regression

## Data

The original submission included intermediate CSV files generated during collection and preprocessing. They are not redistributed in this public portfolio repository.

The collection scripts show how the project obtained match information from Tennis24. Because website structure, browser drivers, and page markup can change over time, the scraping scripts are preserved primarily as coursework and portfolio artifacts rather than as a guaranteed current scraping interface.

## Project Context

This was a **team final project** for the Fall 2024 course **Data Analysis in Sport Science (SPT5062)**. This repository preserves the R scripts included in my submitted project package, with local machine paths removed for public release.

This repository was later organized and published on GitHub for archival and portfolio purposes.
