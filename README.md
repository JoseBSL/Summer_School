# Summer School: Network Analysis in R

Materials for the **IMEDEA Summer School practical on ecological network analysis in R**.

This practical provides an introduction to the preparation, visualization, and analysis of **plant–pollinator interaction networks** using R.

## Repository structure

The practical is organized into three R scripts:

* **`0.install.packages.R`** — Install the packages required for the practical.
* **`1.data.preparation.R`** — Load and prepare the plant–pollinator interaction data.
* **`2.network.visualization.R`** — Visualize and explore plant–pollinator networks.

The **`Data/`** folder contains the datasets used during the practical.

## Dataset

We use plant–pollinator interaction networks from **Kaiser-Bunbury et al. (2017)**, collected on Mahé Island, Seychelles.

The dataset contains **64 plant–pollinator networks** sampled across eight inselbergs between September 2012 and April 2013. Approximately three hours of pollinator observations were conducted per network.

The original dataset and further information are available from the [Interaction Web DataBase](http://www.ecologia.ib.usp.br/iwdb/html/kaiser-bunbury_et_al_2017.html).

## Getting started

Download or clone this repository and open the project in RStudio.

Run the scripts in numerical order:

```text
0.install.packages.R
1.data.preparation.R
2.network.visualization.R
```

The first script installs the packages required for the practical. Once the packages are installed, you do not need to run it again.

## R packages

The practical uses several R packages, including:

* `readr`
* `dplyr`
* `tidyr`
* `ggplot2`
* `viridis`
* `bipartite`


