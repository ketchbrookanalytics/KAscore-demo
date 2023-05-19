# Demo Materials for {KAscore}

This repository contains materials associated with demonstrations of the [{KAscore}](https://ketchbrookanalytics.github.io/KAscore/) R package.

## Installation

We recommend that you use [{renv}](https://rstudio.github.io/renv/) with this project.

To install {KAscore} in a {renv}-friendly way, we recommend creating a folder at `~/renv/cellar/`, and placing the tarball file there (i.e., `/renv/cellar/KAscore_1.0.0.tar.gz`).

Once you have done this, you can install {KAscore} and all other dependencies for the scripts in this repository by running:

```
renv::activate()
renv::restore()
```