# ralmass
[![Travis-CI Build Status](https://travis-ci.org/LDalby/ralmass.svg?branch=master)](https://travis-ci.org/LDalby/ralmass)

The *ralmass* package contain R functions used to prepare GIS data for use in the individual based model system [ALMaSS](http://almass.dk).
It also provides functions to handle output from ALMaSS simulations.

ALMaSS it self is hosted on ccpforge. See more on the [ALMaSS project page](http://ccpforge.cse.rl.ac.uk/gf/project/almass/).

*ralmass* is under development. Use with caution.


## Installation
Installing the latest version of *ralmass* requires having the [*devtools*](https://cran.r-project.org/web/packages/devtools/index.html) package installed. Make sure you have that (on Win you need to install [*RTools*](https://cran.r-project.org/bin/windows/Rtools/) first) and then simply run the lines below:
```s
library(devtools)
install_github('LDalby/ralmass')
```