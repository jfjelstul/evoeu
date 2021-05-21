# evoeu

An `R` package for the Evolution of European Union Law (EvoEU) Database. The database contains over 365,000 legal documents, covering nearly the entire corpus of EU law (1951-2015), and records over 900,000 connections between them. The database includes a `nodes` dataset, where each node is a document, and an `edges` dataset, which each edge is a connection between two documents. 

Abstract: The European Union legal system is one of the most complex and sophisticated in the world. This article models the Acquis Communautaire (i.e. the corpus of European Union law) as a network and introduces the Evolution of European Union Law data set, which tracks connections between European Union primary law, European Union secondary law, European Union case law, national case law that applies European Union law, and national law that implements European Union law. It is the largest, most comprehensive data set on European Union law to date. It covers the entire history of the European Union (1951–2015), contains over 365,000 documents, and records over 900,000 connections between them. Legislative and judicial scholars can use this data set to study legislative override of the Court of Justice of the European Union, the implementation of European Union law, and other important topics. As an illustration, I use the data set to provide empirical evidence consistent with legislative override.

## Installation

You can install the latest development version of the `evoeu` package from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jfjelstul/evoeu")
```

## Documentation

The codebook for the database is included as a `tibble` in the `R` package: `evoeu::codebook`. The same information is also available in the `R` documentation for each dataset. For example, you can see the codebook for the `evoeu::nodes` dataset by running `?evoeu::nodes`. You can also read the documentation on the [package website](https://jfjelstul.github.io/evoeu/).

## Citation

If you use data from the `evoeu` package in a project or paper, please cite the following article in European Union Politics:

> Fjelstul, Joshua C. 2019. "The evolution of European Union law: A new dataset on the Acquis Communautaire." European Union Politics 20(4):670-691.

The bibtex entry for the article is:

```
@article{Fjelstul2019,
    title={The evolution of European Union law: A new dataset on the Acquis Communautaire},
    author={Fjelstul, Joshua C.},
    journal={European Union Politics},
    volume={20},
    number={4},
    pages={670--691},
    year={2019}
}
```

Please also cite the `R` package:

> Joshua Fjelstul (2021). evoeu: The Evolution of European Union Law (EvoEU) Database. R package version 0.1.0.9000.
> 
The `BibTeX` entry for the package is:

```
@Manual{,
  title = {evoeu: The Evolution of European Union Law (EvoEU) Database},
  author = {Joshua Fjelstul},
  year = {2021},
  note = {R package version 0.1.0.9000},
}
```

## Problems

If you notice an error in the data or a bug in the `R` package, please report it [here](https://github.com/jfjelstul/evoeu/issues).
