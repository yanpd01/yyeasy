# yyeasy

## Introduction
A small toolbox for easy use with R. 

## Installation tutorial
```r
## from github
remotes::install_github("yanpd01/yyeasy")

## from gitee
remotes::install_git("https://gitee.com/yanpd01/yyeasy")

## install development version
remotes::install_github(
    "yanpd01/yyeasy",
    ref = "develop",
    upgrade = "never"
)
```
## Function
```r
## Read and write files.
yyread()
yywrite()

## Save plots
yydev()
yyexport()

## Plot function
pca_plot()
pcoa_plot()
nmds_plot()

## ggplot2 themes
theme_bw2()
theme_classic2()

## Packages function
yyinstall()
yyload()
yyunload()
yyuninstall()

## Maps function
coord_get()
coord_rev()
coord_trans2gaode()

## Coefficient and significance marker
get_corr()
get_corr_sig()
get_sig()
```