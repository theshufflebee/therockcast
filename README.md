# The RockCast: University Forecasting Project

## Overview

This repository contains all files related to a forecasting project on ECB Policy rates.

## Usage

In order our code, one only needs to set their FRED API key in their R system environment, as " FRED_API_KEY = your_key_here ".
Then, running the "main.R" script should reproduce all our analysis and figures. 

## Project Structure

```
therockcast/
├── main.R                              # Main file that runs all subsequent scripts
├── scripts/                            # Contains all subscripts and subsubscripts
  ├── 01_taylor_rule_formulas.R         # Subscript that defines the 4 Taylor Rule formulas we use     
  ├── 02_data.R                         # Subscript that creates and analyzes our data
  ├── 03_taylor_rule.R                  # Subscript that runs all initial Taylor Rule estimation
  ├── 04_evaluation.R                   # Subscript that runs the pseudo out-of-sample evaluation scripts for our forecasting model
  ├── 05_final_forecast_script.R        # Subscript that runs our final actual forecast 
```

## Options

There is some customization possible before running the project. These are all offered as options which can be triggered on and off. Below is a list of all available choices and their effect. 

* Use Hamilton Filter: 
  + TRUE: Selects Hamilton method for output gap estimation
  + FALSE: Selects Hodrick-Prescott method for output gap estimation

* Use Inflation Expectations:
  + TRUE: The models used for forecasting will use 12-month ahead inflation
          expectations from the ECB survey of professional forecasts (average).
  + FALSE: The models used for forecasting will use realised inflation
  
* Use Formula
  + Formula 1: Actual interest rate regressed on inflation and output gaps  
  + Formula 2: Shadow interest rate regressed on inflation and output gaps
  + Formula 3: Actual interest rate regressed on the one-quarter lag of the interest
                rate and on inflation and output gaps
  + Formula 4: Shadow interest rate regressed on the one-quarter lag of the shadow 
                interest rate and on inflation and output gaps
                
* Format:
  + html: For printing tables in html (to view in console) 
  + latex: For saving and knitting tables to LaTeX and pdf

* Save figures:
  + TRUE: All figures (plots and tables) are saved to the figures/ folder
  + FALSE: No figures are saved, useful for running the code in console quickly







