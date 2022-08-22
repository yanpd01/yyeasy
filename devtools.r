
## devtools
devtools::document(roclets = c("rd", "collate", "namespace"))
devtools::check()
devtools::load_all()
devtools::unload()
devtools::build()
'
## git base cmd
git add .
git commit -m "optimize devtoos.R label" # 将文件提交到本地仓库
git push -u origin develop  ## push the develop repos 
git checkout develop      ## switch to develop branch
git checkout master       ## switch to master branch
git merge --no-ff develop ## merge develop to master

## add tag
git tag V0.0.3.9                      ## add git tag.
# git tag -a v1.2 9fceb02 -m "my tag"  ## add tag for sepcific commit.
git push origin --tags               ## push all tags to remote.
git push origin v1.0                 ## push specific tag to remote.

## delete tag
git tag -d tag_name                  # delete local tag
git push origin :refs/tags/0.0.2.7   # delete remote tag

cd ..
"C:\Program Files\R\R-4.1.2\bin\x64\Rcmd.exe" INSTALL \
    --no-multiarch \
    --with-keep.source \
    --library="C:/Users/cuilab/Documents/R/win-library/4.1" \
    yyeasy
'

### usethis
usethis::use_package("foreach")
usethis::use_r("beta")
usethis::use_data()

## install
remotes::install_github(
    "yanpd01/yyeasy",
    ref = "develop",
    upgrade = "never"
)


## build R package

## 0、prepare
devtools::has_devel()

## 1、create pkg project -------------------------------------------------------
usethis::create_package("tmp001")

## 2、open the pkg dir; open the pkg project, write functions ------------------
library(devtools)
library(roxygen2)
usethis::use_r("reexport") ## 导入和导出
usethis::use_r("function") ## 编写功能
usethis::use_data(its) ## 先将本地数据导入 R 中变量，然后用这个函数将数据导入到包中
usethis::use_r("data") ## 为数据编写R文件

## 3、annotation --------------------------------------------------------------
yourname <- function() {
    ##  这个位置按下 Ctrl+Alt+Shift+R 插入注释框架
}
devtools::document() ## 转义注释

## 4、write the import depend  -------------------------------------------------
usethis::use_package("magrittr") ## 第二个选项默认为 import

## 5、description --------------------------------------------------------------
person() ## ## 直接在 description 文件中修改标题，描述，作者
usethis::use_gpl3_license()

## 6、load check and build -----------------------------------------------------
devtools::load_all() ## 快捷键：Ctrl + Shift + L 相当于library，可以测试当前包
devtools::unload() ##   前面加载的卸载

devtools::check() ## 快捷键  Ctrl + Shift + E
devtools::build() ## 快捷键  Ctrl + Shift + B