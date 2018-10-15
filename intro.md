## Property Investment Market Trend Visualization

### Introduction:

The purpose of this Shiny webApp is to better advise clients in residential property investments in New York City by visualizing sales and rental market trends in major areas.

### Data:

Sales and rental data are compiled from [OneBlockOver by Streeteasy](https://streeteasy.com/blog/download-data/). The dataset used in this webApp is combined using: Borough, Neighborhood, Date, Condo Listing Median Price, Condo Sale Median Price, Rental Studio Median Price, Rental One Bedroom Median Price, Rental Two Bedroom Median Price, Rental Three+ Bedroom Median Price.

Boroughs in consideration are: Manhattan, Brooklyn, Queens.
The date of the data point ranges from Jan, 2014 to Aug, 2018, aggregated monthly.

Boundary data of the neighborhoods are acquired from [data.beta.nyc](http://data.beta.nyc).

## Why Median Price:

The median is the value separating the higher half from the lower half of a data sample. Median is less affected by outliers and skewed sample which is typical in real estate data. It is a more accurate indicator of the market. 

## How to use this webApp:

Start by clicking on the "Visualizaitons" tab on the side menu to go to the visualization page. Beware that it takes a little time to load so don't be startled by the error messages. Patience and voilà.

First, choose a borough and neighborhood from the right side of the page.

Then, you will see that the neighborhood you selected will be highlighted on the map at the top of the page. 

On the bottom left, there will be a brief summary of the market trend of property sales in your chosen neighborhood, whether it has been increasing or decreasing. And it will provide a rough estimate on percentage change in property value, and value prediction for 2019 and 2023 (5 year). You can also opt to see an interactive chart of Listing and Sale price over the years.

On the bottom right, there is a chart showing the rental prices for different properties types, whether it is a Studio, One Bedroom, or even Three+ Bedroom.

You will notice a slider and selector below the location selectors, it is used to set a budget and a room count so that the tool will calculate a rough estimate of your mortgage payment, given a 30 year term and an annual rate of 5.04%. 

## Future Works:

- Still working on the map feature. The ideal is to show nearest neighborhood info on the map for comparison.
- Dataset is not really complete for neighborhoods that brokers do not post listings on Streeteasy. Need more detailed and comprehensive data.
- Need more features to consider such as crime rate, school ranking, etc. for different neighborhoods. Should be a great web scraping project to work on.