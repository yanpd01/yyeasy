# yyeasy

## Introduction
A small toolbox for easy use with R. 

## Installation tutorial

```r
## from github
remotes::install_github("yanpd01/yyeasy", upgrade = "never")

## from gitee
remotes::install_git("https://gitee.com/yanpd01/yyeasy", upgrade = "never")


```
## Function
```r
## Read and write files.
yyread()
yywrite()

## Save plots
yydev()
yysave()

## Plot function
pca_plot()
pcoa_plot()
nmds_plot()

## compute alpha indexs and beta dist.
alpha_index()
beta_dist()

## ggplot2 themes
theme_bw2()
theme_classic2()

## Packages function
yyinstall()
yyload()
yyunload()
yyuninstall()

## Maps function
maps_get_address()
maps_get_elevation()
maps_get_coords()
maps_trans2gcj02()

## Coefficient and significance marker
get_corr()
get_corr_sig()
get_sig()
```