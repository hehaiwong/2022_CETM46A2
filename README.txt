CETM46 R + Shiny + Dashboard by Helen Wong Hoi Lun
Instruction for running the photo type package in local PC
1.	To run the R-script and R Shiny locally, firstly download 1 app.R file (core program ) and 2 .csv files ,(history,user.csv)    where are the dataset  for account and history log , from GitHub https://github.com/hehaiwong/2022_CETM46A2 .
2.	Open the R Studio.
3.	Please set your “Working directory” point to the folder where having downloading file in point #1.
4.	Please install the following packages in R Studio
a.	DT()
b.	Shinydashboard()
c.	shiny()
d.	shinyWidgets()
e.	ggplot2()
f.	leaflet()
g.	tidyverse()
h.	gganimate()
Run the command in R Studio console:-
install.packages(c("shiny", "DT", "shinydashboard", "leaflet", "tidyverse", "gganimate"))

5.	Select the “Run App” button and choose the option “Run external”, then press the button “Run App”
6.	New browser will be launched and E-Catch analysis display and ready for user to review or submit the new request.

================================================================================
Special thanks to the data provider  for this data product (prototype) 1) My information bubble(MIB)  http://mib.projects.iit.cnr.it/dataset.html who has clean, managed and verify the original data set provided by 2) WWW '17 Companion: Proceedings of the 26th International Conference on World Wide Web Companion, https://dl.acm.org/doi/10.1145/3041021.3055135
=================================================================================
----Helen Wong Hoi Lun 
