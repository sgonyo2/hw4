**Description**  
The purpose of this project is to create a [shiny app](https://sgonyo.shinyapps.io/dashboard/?_ga=2.179947940.1663124088.1682603557-1101961854.1682603557).
 to explore data from the [World Value Study (WVS)](https://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp).
The reader is able to select a country from a drop-down menu and click between the tabs to explore the attitudes related to democracy, news consumption, and attitudes related to science.
The averages for the selected country are provided in a table and a graph, and the averages for the entire WVS sample are provided in a graph, as well.

**Organization**  
[data_prep](https://github.com/sgonyo2/hw4/blob/main/data_prep.Rmd) imports and prepares the data.  
[dashboard](https://github.com/sgonyo2/hw4/blob/main/dashboard.Rmd) creates the [shiny app](https://sgonyo.shinyapps.io/dashboard/?_ga=2.179947940.1663124088.1682603557-1101961854.1682603557)


**Session Info**  
R version 4.2.3 (2023-03-15 ucrt)  
Platform: x86_64-w64-mingw32/x64 (64-bit)  
Running under: Windows 10 x64 (build 19044)  

Matrix products: default  

locale:  
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

attached base packages:  
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:  
[1] rsconnect_0.8.29     readr_2.1.4          shinydashboard_0.7.2 shiny_1.7.4          ggplot2_3.4.2        tidyr_1.3.0          dplyr_1.1.1         
[8] labelled_2.11.0      haven_2.5.2 
