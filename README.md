# Product-Forecasting

This R Shiny application uses confidential production and sales data of a company to be confidential. 
The goal is to build an inhouse forecasting tool for analyzing aggregated datasets with multiple forecasting techniques. 
Since this is a business problem, the problem was approached in 3 steps:

Initial Analysis:

This part of the analysis utilizes the power of ggplot to draw the insights from the data. These plots are used to arrive 
at the customer-product combination with defined time interval of weeks, months, quaters and years.

Segmentation Analysis:

As per the client requests, I changed the perspective to a broader view in segmentating the customers based on the frequency 
and most contributing customers via ABC analysis.

Time Series Forecasting:

After extensive research on initial analysis and segmentation, I built on to Naive, Moving Average(MA, Exponential 
Time Smoothing(ETS), Auto-Regression and Moving Average(ARIMA), Hybrid Model(Combination of ARIMA and ETS parameters).
We companred the results and developed a RShiny application for team usage.

Metrics used - Absolute Percentage Error (APE)

Using my code:

1. Import all the necessary libraries
2. Connect all the source files required which performs the core-logic with plots and accuracy calculation
3. Click on, 'RunApp' forking my app.R
