yapomif
[![Build Status](https://travis-ci.org/lbraglia/yapomif.svg)](https://travis-ci.org/lbraglia/yapomif)
[![Build status](https://ci.appveyor.com/api/projects/status/0eho7pv20bdgisue)](https://ci.appveyor.com/project/lbraglia/yapomif)
=======

Aka "Yet another package of misc functions": an
[R](http://www.r-project.org/) package with 
miscellaneous function (wrappers for data input, data analysis
utilities etc).

It's mainly intended for personal use (aka backward compatibility
is not an issue).


## Install

You can install this package via GitHub. Before that, you
need to setup `devtools` 

```R
install.packages("devtools", dependencies=TRUE)
```

Then:
```r
install_github("yapomif", "lbraglia")
```
On *some Unix platform* `install_github` has been [reported](https://github.com/hadley/devtools/issues/467) not to
work as expected. A handy workaround, in the meantime, could be the following
simple bash script (eg named `r_install_github`):

```bash
#!/bin/bash

cd /tmp && \
rm -rf R_install_github && \
mkdir R_install_github  && \
cd R_install_github && \
wget https://github.com/$2/$1/archive/master.zip && \
unzip master.zip
R CMD build $1-master && \
R CMD INSTALL $1*.tar.gz && \
cd /tmp && \
rm -rf R_install_github
```

and to install `yapomif` (after giving execution permissions and
putting it in a `PATH` directory):
```bash
r_install_github yapomif lbraglia
```
