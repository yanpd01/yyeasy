# yyeasy

## Introduction
A small toolbox for easy use with R. 

## Installation tutorial
#### from github
```r
remotes::install_github("yanpd01/yyeasy")
```
#### from gitee
```r
remotes::install_git("https://gitee.com/yanpd01/yyeasy")
```
#### install development version<br>
(devtools relies on the remotes package  )
```r
devtools::install_github(
    "yanpd01/yyeasy",
    ref = "develop",
    upgrade = "never"
)
```
## Function
### Read and write files.
yyread()<br>
yywrite()

### Save plots
yydev()<br>
yyexport()<br>

### Plot function
pca_plot()<br>
pcoa_plot()<br>
nmds_plot()<br>

### ggplot2 themes
theme_bw2<br>
theme_classic2<br>

### Packages function
yyinstall()<br>
yyload()<br>
yyunload()<br>
yyuninstall()<br>

### Maps function
coord_get()<br>
coord_rev()<br>
coord_trans2gaode()

### Coefficient and significance marker
get_corr()<br>
get_corr_sig()<br>
get_sig()<br>
