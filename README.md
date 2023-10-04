# entropies --- Entropy Triangles

An R project to work with entropic coordinates, entropy triangles, NIT and EMA as defined in: 

- [(2010) Two information-theoretic tools to assess the performance of multi-class classifiers](https://www.sciencedirect.com/science/article/pii/S0167865510001662)

- [(2014) 100% Classification Accuracy Considered Harmful: The Normalized Information Transfer Factor Explains the Accuracy Paradox](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0084217)

- [(2017) The Evaluation of Data Sources using Multivariate Entropy Tools](https://www.sciencedirect.com/science/article/pii/S0957417417300805)

- [(2018) Assessing Information Transmission in Data Transformations with the Channel Multivariate Entropy Triangle](https://www.mdpi.com/1099-4300/20/7/498)

- [(2020) A framework for supervised classification performance analysis with information-theoretic methods](https://ieeexplore.ieee.org/document/8709846)

Uses packages `ggtern` and `entropy` to provide basic functionality.

Forever in beta testing, since this is the main tool to help me develop the theory, for the time being. At present you can:

- measure the performance of supervised classification on multiclassifiers (finished. Papers: 2010, 2014, 2020). Also applies to binary channels. 

- measure the information content of multivariate sources (Discrete sources finished. In progress to improve measurements on continuous sources. Paper: 2017)

- measure the information transmission in multivariate data transformations (Still experimental. Paper: 2018)

You may find the theoretical framework, latest news and further support on the project overview:

- [Evaluation of Machine Learning Techniques by Entropic Means](https://www.researchgate.net/project/Evaluation-of-Machine-Learning-Techniques-by-Entropic-Means)


# Usage and Examples

You can see [here some use cases](vignettes/) in this link, where you can find some examples of real case scenarios.

# Related packages

## Matlab Entropy Triangle Package (2010)

This package was developed by me as the original tool (e.g used in the 2010 paper)

[Matlab Entropy Triangle Package](https://es.mathworks.com/matlabcentral/fileexchange/30914-entropy-triangle)

The following packages have been developed by students of mine with my approval 

## Weka Entropy Triangle plug-in (2015)

There is a plug-in for Weka that lets you evaluate binary classifiers with the ET.

[Weka ET plug-in](https://apastor.github.io/entropy-triangle-weka-package/)

## Python Entropy Triangle Package (2018)

[![PyPI version](https://badge.fury.io/py/entropytriangle.svg)](https://badge.fury.io/py/entropytriangle)
[![Anaconda-Server Badge](https://anaconda.org/jaimedlrm/entropytriangle/badges/version.svg)](https://anaconda.org/jaimedlrm/entropytriangle)

A Python package to work with entropic coordinates and the entropy triangles.

## Interested in porting the ET to your favourite language?

Contact me and I will tell you other initiatives that I am aware of.
