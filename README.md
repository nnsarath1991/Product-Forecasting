# Product-Forecasting

This R Shiny application uses confidential production and sales data of a company. 
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

RShiny Link:
https://sarathkumarnallusamyrshiny.shinyapps.io/Product_Forecasting/

The speciality of this app is REPRODUCIBILITY:

You can choose you own file having time series data with following conditions and follow these step-wise:

NOTE: I've displayed variable names in the drop-downs. Please select them as per the following instructions!

1. Upload your CSV file
2. Select one filtering variable - could be a region, a factory or any variable that could be filtered.
3. Select the filtering value of the selected filtering variable name in step 2.
4. Select the Time series feature - Week, Month, Quarter in 'YYYY-MM-DD' format
5. Select the starting range. This step is to analyze during desired time period say, 2 years
Example: If you have data from 2014 March till 2018 March, you can give start date as 2015 Jan
6. Select the ending range. 
Example: Continuing above example, you can choose 2017 Dec to get annaul model for prediction making more sense.
7. Select Product variable - could be an uniqueID for each product
8. Select the Customer variable -  could be an uniqueID for each customer
Note: This Product-customer level combination is critical for you business decision
9. Select the Volume variable - This is final variable having quanity from the past
10. Select type of SKU, you would like to analyze as follows,

a) Regular SKUs - most frequent SKU based on our business call. Here, it referes to the transaction in the selected time range.
b) Other SKUs - less frequent. Here, it is been coded as transactions on average in a selected time range
c) All SKUs - Combines a) and b) to give the overall model performance

11. Select Forecasting horizon for the model to predict as per your case.
12. Go to other tabs,
 -> SMA, Simple Moving Average choose order and click on 'Click to See Future' for Forecast plot and APE accuracy
 -> ETS, Exponential Time Smoothing, choose the parameters
 -> ARIMA, Auto regression Integrated Moving Average
 -> Hybrid, Combination of ETS and ARIMA, my novel idea part of this project!
 13. Used the results for forecasting the right product-customer combination using my powerful analysis tool!!
 
 RShiny Link -  https://sarathkumarnallusamyrshiny.shinyapps.io/Product_Forecasting/
 
 Thanks for using my Application!
 


Comments are welcome!
