---
title: "Working with UserTests"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---

Start by properly preparing your data file, which should be saved in``.csv`` format. The data file should have either 3 or 4 columns, depending upon whether one or two test groups are being analyzed. 

In both cases, three of the columns should be: **Participant**, **Task**, and **Success**. The fourth (optional) column shuold be **Group**. See table 1:

Table 1

Participant | Task | Success | Group   
----------- | ---- | ------- | ----- 
 1          | 1    |  1      | CTCT    
 1          | 1    |  0      | MC      
 1          | 2    |  1      | CTCT    
 1          | 2    |  1      | MC      
 2          | 1    |  1      | CTCT    
 2          | 1    |  0      | MC      
 2          | 2    |  0      | CTCT    
 2          | 2    |  1      | MC      

Next, import the file into R.

```{r}
mydata<-readr::read_csv("sample data/sample_data_2.csv")
```

Next, isntall and load the ``UserTests`` package.

```{r}
install.packages("devtools")
library(devtools)
devtools::install_github(repo = )
```

