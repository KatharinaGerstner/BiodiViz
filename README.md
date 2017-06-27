# **mobsim_app** -- Simulation and visualization of biodiversity patterns across spatial scales using the `mobsim` R-package 

## Authors

*Katharina Gerstner, Felix May*

## Description

This is an interactive tool that aims to simulate and visualize multiple biodiversity patterns across spatial scales using the `mobsim` R-package. A **Thomas model** is used to simulate point pattern distributions of individual species. Key parameters of the simulation are:

* **total number of individuals**, 
* **total number of species**, 
* the coefficient of variation in the species-abundance distribution that determines the **eveness of the abundances**,
* **spatial aggregation of species**.

The tool   
1. simulates locations of individuals of different species in a location (plot, area);      
2. plots biodiversity patterns such as   
+ species-abundance distributions (SAD) using Preston octave plot and rank-abundance curve,   
+ spatial distribution of individuals within a unit area,  
+ species accummulation curves (SAC), species-area relationships (SAR), and the distance-decay curve.   


## Run the App

The app thus depends on the R-package `mobsim`. The latest version of `mobsim` will be automatically downloaded from the GitHub repository 'MoBiodiv/mobsim' using the R-package `devtools`.  The interactive interface relies on the R-package `shiny`.

To run the app two lines of code are needed

> library(shiny)  
> runGitHub("KatharinaGerstner/mobsim_app")


## License

GNU GPL




