# ERTH 162 - Spring 2025 - Project #1: Data Collection

This repository contains the code and data for Project #1 of ERTH 162, focusing on data collection and analysis.

## Project Overview

The primary goal of this project is to analyze weather data for Waikiki, Honolulu, HI, using the `data.csv` dataset.

## Files

*   `main.R`: The main R script that performs data loading, preprocessing, analysis, and visualization.
*   `data.csv`: The raw weather data used for the analysis.
*   `plots/`: Directory containing the generated plots.
*   `changelog.md`: A log of changes made to the project.
*   `file-explanation.md`: Explanations for the files in this repository.

## Analysis

The `main.R` script performs the following:

1.  Loads and preprocesses the weather data.
2.  Generates time series plots for temperature, dew point, relative humidity, and precipitation.
3.  Calculates monthly summaries of weather variables.
4.  Compares the 2024 data with historical climatology data for Honolulu.
5.  Generates anomaly plots for temperature and precipitation.
6.  Calculates and prints key weather statistics for the period.

## Usage

To run the analysis, execute the `main.R` script in an R environment with the required libraries installed (`ggplot2`, `dplyr`, `lubridate`, `zoo`, `patchwork`, `scales`). Ensure the `data.csv` file is in the same directory. 