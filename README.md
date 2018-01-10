# **BiodiViz** -- Visualizing biodiversity patterns across spatial scales

## Authors

*Katharina Gerstner, Petr Keil*

## Description

This is an interactive tool that aims to visualize multiple
biodiversity patterns at multiple spatial scales. The tool 
(1) simulates locations of individuals of
different species in a location (plot, area); the key parameters of the simulation 
are **total number of individuals**, **total number of species**, **eveness of the
abundances** and **aggregation of species**.   
(2) plots biodiversity patterns such as species-abundance distributions (SAD), species
accummulation curves (SAC), species-area relationships (SAR) and alike for a selected grid cell and for the entire region. 

The package that is used to simulate the community is **Felix May**'s `MoBspatial`. 
We use **Thomas model** to simulate point pattern distributions of individual species.

The interactive interface relies on package `manipulate`.

## License

GNU GPL




