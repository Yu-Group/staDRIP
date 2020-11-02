# A stability-driven protocol for drug response interpretable prediction (staDRIP)

This repository contains all code, results, and documentation to accompany Xiao Li, Tiffany M. Tang, Xuewei Wang, Jean-Pierre Kocher, Bin Yu ''A stability-driven protocol for drug response interpretable prediction (staDRIP)'' (2020). 

In this work, we develop staDRIP, a transparent stability-driven pipeline to predict a cell line's response to therapeutic drugs given its unique -omic profile. This pipeline builds upon the PCS framework for veridical data science [Yu and Kumbier, 2020](https://www.pnas.org/content/117/8/3920) and helps to mitigate the impact of human judgment calls throughout the scientific knowledge extraction process.


## PCS Documentation

We acknowledge that there are inevitably many human judgement calls that can impact the prediction accuracies and interpretations of our fitted personalized drug response models. We thus detail our decisions and provide a reproducible script with all of our results in this [PCS documentation](https://github.com/Yu-Group/staDRIP/blob/master/ccle_variance_filtered.html). The Rmarkdown source code to generate the html PCS documentation can be found [here](https://github.com/Yu-Group/staDRIP/blob/master/ccle_variance_filtered.Rmd).


## Project File Structure

- *ccle_variance_filtered.html*: main PCS documentation
- *ccle_variance_filtered.Rmd*: Rmarkdown source code to generate main PCS documentation
- *bibliography.bib*: bib file for PCS documentation
- **bmtmkl**: code to run Bayesian Multitask Multiple Kernel Learning method
- **css**: css style files for PCS documentation
- **data**: CCLE data sets
- **functions**: contains helper functions
- **images**: additional images in PCS documentation
- **results**: saved results from fitted models
- **xvae**: code to run X-shaped Variational Autoencoders



