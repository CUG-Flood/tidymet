
<!-- README.md is generated from README.Rmd. Please edit that file -->

# missInfo

[![Travis Build
Status](https://travis-ci.org/kongdd/missInfo.svg?branch=master)](https://travis-ci.org/kongdd/missInfo)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/kongdd/missInfo?branch=master&svg=true)](https://ci.appveyor.com/project/kongdd/missInfo)
[![codecov](https://codecov.io/gh/kongdd/missInfo/branch/master/graph/badge.svg)](https://codecov.io/gh/kongdd/missInfo)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![CRAN](http://www.r-pkg.org/badges/version/missInfo)](https://cran.r-project.org/package=missInfo)

`missInfo` package is used to detect daily meteorological station data
missing information and interpolate missing values.

-----

**Tasklist**:

  - [ ] Add QC\_flag for filled value

## Installation

You can install the released version of missInfo from [github](https://github.com/kongdd/missInfo) with:

``` r
devtools::install_github("kongdd/missInfo")
```

## Example

This is a basic example which shows you how to solve a common problem:

  - **prcp** data.frame, with dimension of \[n\_date, 1+n\_station\],
    and the first column is date.
  - **st840** data.frame, with columns at least of ‘site’, ‘lat’, ‘lon’
    (degree).

<!-- end list -->

``` r
library(missInfo)
## basic example code
data("prcp")
data("st840")
r <- interp_main(prcp, st840, smax = 200, verbose = FALSE)
#> [0    ] Detect missing information of original data ...
#> [1.1  ] runing na.approx interpolate ...
#> [1.2  ] Detect missing info after na.approx interpolate ...
#> [2.1.1] runing linear lm interpolate ...
#> [2.1.2] Detect missing info after first lm interpolate ...
#> [2.2.1] runing second linear lm interpolate ...
#> [2.2.2] Detect missing info after second lm interpolate ...
#> [3.1  ] History average interpolate...
#> [3.2  ] Detect missing info after hisavg interpolate ...
```

## 气象要素特征值说明 (Illustration of meteorological flags)：

  - 台站海拔高度 +100000 当台站海拔高度为估测值时，在估测数据基础上加100000

  - 各要素项 32766 数据缺测或无观测任务

  - 气压日极值 +20000 气压极值取自定时值，在原值上加20000

  - 日最小相对湿度 +300 最小相对湿度取自定时值，在原值上加300

  - 风速 +1000 当风速超过仪器测量上限时，在上限数据基础上加1000

  - 风向 1-17 用数字表示风向方位，17表示静风 +100 当表示风向为八风向时，在原值上加100 90X
    风向出现X个时，风向数据用个数X表示 95X
    风向至少出现X个时，风向数据用个数X表示

  - 降水量
    
    | flag  | Description                |
    | ----- | -------------------------- |
    | 32700 | 表示降水“微量” (0.1mm)           |
    | 32XXX | XXX为*<u>纯雾露霜</u>*          |
    | 31XXX | XXX为<u>*雨和雪的总量*</u>        |
    | 30XXX | XXX为<u>*雪量(仅包括雨夹雪，雪暴）*</u> |
    

  - 蒸发量  
    32700 表示蒸发器结冰 +1000 蒸发器中注入的水全部蒸发，在注入的水量数据基础上加1000

  - 0cm地温 +10000 实际温度（零上）超仪器上限刻度，在上限数据基础上加10000 -10000
    实际温度（零下）超仪器下限刻度，在下限数据基础上减10000

  - *Quality Control (**QC**) flags*
    
    | flag | Description |
    | ---- | ----------- |
    | 0    | 正确        |
    | 1    | 可疑        |
    | 2    | 错误        |
    | 4    | 定制数据    |
    | 7    | 无观测任务  |
    | 8    | 缺测        |
    | 9    | 未进行质量控制 |
    
    

# **References**

> \[1\] Dongdong Kong, R package: missing information detect and
> interpolate, `missInfo` version 0.1.1,
> <https://github.com/kongdd/missInfo>

# Acknowledgements

Keep in mind that this repository is released under a GPL-3 license,
which permits commercial use but requires that the source code (of
derivatives) is always open even if hosted as a web service.
