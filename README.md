# The RockCast: University Forecasting Project

A comprehensive bookdown environment for learning and applying forecasting methods using R.

## Overview

This repository contains all files related to a forecasting project on ECB Policy rates.

## Usage

In order our code, one only needs to set their FRED API key in their R system environment, as " FRED_API_KEY = your_key_here ".
Then, running the "main.R" script should reproduce all our analysis and figures. 

## Project Structure

```
therockcast/
├── index.Rmd              # Main entry point and preface
├── 01-intro.Rmd           # Introduction to Forecasting
├── 02-chapter-1.Rmd      # Time Series Fundamentals
├── 03-chapter-2.Rmd         # Forecasting Methods
├── 04-chapter-3.Rmd      # Model Evaluation and Selection
├── _bookdown.yml          # Bookdown configuration
├── _output.yml            # Output format configuration
├── book.bib               # Bibliography file
├── style.css              # Custom CSS styling
├── .gitignore             # Git ignore file
└── _book/                 # Generated book output (after building)
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
  + html: For outputting in console or knitting to html
  + latex: For knitting to pdf








