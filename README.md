# eda

## Introduction
The edaR package can be used to automate exploratory data analysis. It currently is able to output basic statistics for individual columns with a simple function call.

The idea is to take that output and print it out in a Rmarkdown document for purusing.

## Installation
Make sure you have `devtools` installed. Then run
```
install_github('vitallish/eda/edaR')

```

## Basics
The brunt of the work is done by the `singleVarStats(x)` where x is a dataframe. edaR decides what type of output to create based on the column types, so be sure to correctly type them.

Also, please note the necessary knitr chunk options in the code below

    ```{r, comment=NA, results='asis', message=FALSE, warning=FALSE}
        
        raw_lists <- singleVarStats(edaR_test) 
        
        printSingleVarStats(raw_lists)
    ```
You can see this in action in the `samp_report.html` file and on the web here: 
http://htmlpreview.github.io/?https://raw.githubusercontent.com/vitallish/eda/master/samp_report.html


## Current Focus
1. Documenting and cleaning up package. There is basically no documentation.

## Up Next
1. 2- Variable Exploration!
