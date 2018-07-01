---
title: "Working with UserTests"
output:
  html_document:
    keep_md: true
    number_sections: yes
editor_options:
  chunk_output_type: inline
---

The functions in this package compute success rates and confidence intervals following [Jeff Sauro's excellent write up on MeasuringU](https://measuringu.com/wald/)

# Install packages
Install and load the ``UserTests`` package and some other packages.


```r
#install.packages("readr", repos = "http://cran.us.r-project.org" )
library(readr)
#install.packages("devtools", repos = "http://cran.us.r-project.org")
library(devtools)
#devtools::install_github(repo = "darrellpenta/UserTests")
library(UserTests)
```

# Quick Analyses

If you just want to run analyses on a single task for which you know the success rate and the number of trials, provide that information to the ``UserTests::completion`` function, as below. 



```r
mydata <-
  completion(.success=7, .trials=17)
mydata
```

```
##   successes trials orig.succ.pct estimator success.pct low.ci.pct
## 1         7     17         41.18    Wilson        42.8      21.56
##   hi.ci.pct
## 1     64.05
```

The results returned indicate:

1. **successes**  
The total number of success
1. **trials**  x  x   
The total number of trials
1. **orig.succ.pct**  
The raw success rate as a percentage
1. **estimator**  
The name of the method used to adjust the sucess rate (see [Lewis & Sauro, 2006](http://uxpajournal.org/wp-content/uploads/pdf/JUS_Lewis_May2006.pdf])).
1. **success.pct**  
The adjusted success rate as a percentage
1. **low.ci.pct**  
The lower confidence limit as a percentage
1. **hi.ci.pct**  
The upper confidence limit as a percentage

# Analyzing larger data sets

## Get your data in order
 
Start by properly preparing your data file, which should be saved in``.csv`` format. The data file should have either 3 or 4 columns, depending upon whether one or two test groups are being analyzed. 

In both cases, three of the columns should be: **Participant** (numeric value), **Task** (numeric value), and **Success** (numeric value, coded as 1=success, 0=failure). The fourth (optional) column shuold be **Group** (character/text value). See table 1:

**Table 1. Example task completion data set**

Participant | Task | Success | Group   
----------- | ---- | ------- | ----- 
 1          | 1    |  1      | US    
 1          | 1    |  0      | THEM      
 1          | 2    |  1      | US    
 1          | 2    |  1      | THEM     
 2          | 1    |  1      | US    
 2          | 1    |  0      | THEM      
 2          | 2    |  0      | US    
 2          | 2    |  1      | THEM
 ...        | ...  | ...     | ...


## Import the data
Next, import the ``.csv`` file into R using the [readr](https://cran.r-project.org/web/packages/readr/README.html) package.


```r
mydata<-
  readr::read_csv("sample data/sample_data_2.csv")
```

```
## Parsed with column specification:
## cols(
##   Group = col_character(),
##   Participant = col_integer(),
##   Task = col_integer(),
##   Success = col_integer()
## )
```

```r
head(mydata)
```

```
## # A tibble: 6 x 4
##   Group Participant  Task Success
##   <chr>       <int> <int>   <int>
## 1 US              1     1       0
## 2 US              1     2       1
## 3 US              1     3       0
## 4 US              1     4       1
## 5 US              2     1       1
## 6 US              2     2       1
```


## Create a table of the adjusted completion rate means and confidence intervals
Run the completion function on your data set to view a table of summarized data. The ``head`` function in the code below is a convenient way to view the first few rows of data.

```r
mytable<-
  completion(mydata)
head(mytable)
```

```
## # A tibble: 6 x 9
## # Groups:   Task, Group [6]
##    Task Group successes trials orig.succ.pct estimator success.pct
##   <int> <chr>     <int>  <int>         <dbl> <chr>           <dbl>
## 1     1 THEM          4      8          50   MLE              50  
## 2     1 US            4      8          50   MLE              50  
## 3     2 THEM          5      8          62.5 MLE              62.5
## 4     2 US            7      8          87.5 MLE              87.5
## 5     3 THEM          2      8          25   Wilson           33.1
## 6     3 US            5      8          62.5 MLE              62.5
## # ... with 2 more variables: low.ci.pct <dbl>, hi.ci.pct <dbl>
```

## Export the table

You can export the table as a ``.csv`` file. Just include the path to the location where you want to save the file to the ``path`` argument in the ``write_csv`` funciton, as in the example below. 

```r
readr::write_csv(mytable, path = "..\MyDesktop\MyUsabilityStudy\completion-rates.csv")
```

# Create a figure
If you need a figure, use the ``comp_figure`` function. You can overwrite the default labels by providing your own to the appropriate arugments in the function.


```r
myfigure <-
  comp_figure(mytable, xlabel="Test Task", ylabel = "Success (%)", legend_lab="Groups")
myfigure
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

# Saving the figure

To save the figure, provide the output format and path. The easiest way is to combine both of these in one string.  Acceptable file formats include:

* eps
* ps
* tex
* pdf
* jpeg
* tiff
* png
* bmp
* svg
* wmf


```r
ggsave("../MyDesktop/usability-test-figure.png")
```

