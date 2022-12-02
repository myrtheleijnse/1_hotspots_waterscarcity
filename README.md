![Dependabot](https://img.shields.io/github/languages/code-size/myrtheleijnse/1_hotspots_waterscarcity)

# Welcome :star2:
Project: Key drivers and pressures of hotspots in water scarcity

Author: M. Leijnse

Contact: m.leijnse@uu.nl

Organization: Utrecht University

## Introduction
Hello, thank you for reading me!

This repository consist of scripts used for the National Geographic World Water Map project. 
In this repository I'm sharing the code used to generate any output on my identification of the global hotspots in water scarcity and their drivers and pressures.
All code is generated in the R language.

## Download
- Install R version 4.2.1 (2022-06-23 ucrt)
- Install RStudio 
	- For dependencies, see [renv.lock](https://github.com/myrtheleijnse/1_hotspots_waterscarcity/renv.lock)

Download
```
git clone git@github.com:myrtheleijnse/1_hotspots_waterscarcity.git
```

## Project organization
- PG = project-generated
- HW = human-writable
- RO = read only
```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```


## License
This project is licensed under the terms of the [MIT License](/LICENSE.md)
