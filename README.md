---  
dev version: 0.0.0.9000  
---  

# receptors  

This R package generates and edits simple point receptor grids for EPA's air dispersion model _AERMOD_.  

Grid options include:  

- Polar  
- Circle  
- Rectangle  
- Rings (donuts)    

Receptor files can be exported in AERMOD's _.rou_ text format.

## Install 

```r
install.packages("remotes")

remotes::install_github("dKvale/receptors")
```

## Use

Let's create an AERMOD receptor file: `receptors.rou`. We can set the grid to have circles of receptors at every 5 meters from 5 to 100 meters from the center point.
```r
library(receptors)

recepts <- polar_grid(radii = seq(5, 100, 5))

write_rou(recepts, "receptors.rou")
```
