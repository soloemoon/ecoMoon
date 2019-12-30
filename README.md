# ecoMoon: R package to quickly download, clean, visualize and transform economic data
This package allows for economic data to be extracted from a specified source (currently only FRED supported...more coming soon). Output of data can be specified to include recession bars, be formatted as a time series, and be outputted in either long or wide format.

This package is currently only available from GitHub. CRAN installation coming soon. You can install this package via Hadley's devtools with the following code:

devtools::install_github("soloemoon/ecoMoon")

## eco.download

The eco.download function allows for data to be extracted from a specified source (currently only FRED supported...more coming soon). Options allow for the inclusion of recession bars, time series output, and output in either the long or wide format.

eco.download(keys = c('UNRATE', 'PAYEMS'), key.rename = c('UE','Payrolls'), shape = 'w', show.recession = 'y',time.series = 'n') 
  * keys are the FRED identifiers. 
  * key.rename are the user inputted names corresponding to each FRED identifier. 
  * shape specifies if the dataframe should be outputted in the long (keys ins rows) or the wide (keys in columns) format. The default        is wide.
  * show.recession determines whether to include or exclude recession indicators (only available for monthly, daily, or quarterly data).      Weekly transformation coming soon. Default is to include recession indicator.
  * time.series determines whether the output of the data will be a dataframe or xts object. Default is 'n' meaning data is outputted as      dataframe.

## eco.transform

The eco.transform function allows for data to be transformed using the selected transformation type. Transformations are applied to the entire dataframe.

eco.transform(df = data, transformation = '% change', lags = 1)
  * df is the dataframe (or xts) containing the economic data. It is reccomended to use the eco.transform function in coordination with      the eco.download output only.
  * transformation is the type of transformation to be applied to the dataset.
      Transformations include:
        1. % change
        2. difference
        3. log
        4. annualize monthly
        5. annualize quarterly
      (more coming soon)

## Plotting Tools

Several pre-formatted plotting functions have been included. Each function includes plotting of recession bars. The plots are all GGplot2 and GGTheme based. Options to change the formatting coming soon. Plots included:
   * eco.plot - plot a single economic data series (line chart)
   * eco.bar - plot series in bar format (bar chart)
   * eco.plot2 - plot two economic series (line chart)
   * eco.plot3 - plot three economic series (line chart)
   * eco.barline - plot two economic series: one as  a line and the other as a bar.
   
Thanks to all the developers of packages such as dplyr, purrr, fredr, etc whose hard work made this package possible!

Features coming soon: 
1. Date Filtering
2. Bloomberg data pull
3. Quandl Data pull
4. More chart themes and added flexibility
5. batch dataset etl (BEA and BLS)
