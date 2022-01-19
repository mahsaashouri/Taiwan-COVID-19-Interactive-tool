# Taiwan-COVID-19-Interactive-tool
The COVID-19 data analysis is essential for policymakers to analyze the outbreak and manage the containment. Here, we propose a web-based interactive tool to cluster and forecast the available data of Taiwan COVID-19 confirmed infection cases. We apply the Model-based (MOB) tree and domain-relevant attributes to cluster the dataset and display forecasting results using the Ordinary Least Square (OLS) method (using ```olsfc.single.R``` function). 

Our dataset is ```covid19_tw.csv```, which shows the daily Taiwan COVID-19 confirmed cases in different cities, townships, and districts from 2021-01-01 to 2021-06-04. Domain-relevant attributes applied to this dataset include geographical division, _region_ (6 categories), _administrative_ (3 categories),  _population_ (numeric), _imported_ (2 categories), and _airport_ (2 categories). To run the Shiny tool, one can run ```server.R``` (the instructions) and ```ui.R``` (the user interface) together in the same folder with the ```covid19_tw.csv``` and ```olsfc.single.R``` files. 

The app screenshot using MOB depth = 3, Prune option = 'AIC' with all the above domain-relevant attributes as splitting variables:

![alt text](<https://github.com/mahsaashouri/Taiwan-COVID-19-Interactive-tool/blob/main/screenshot-Taiwan-Interactive-tool.png>)


Binder 
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/mahsaashouri/Taiwan-COVID-19-Interactive-tool/master?urlpath=shiny)
