beeplotR
========

## Introduction
The beeswarm plot is a one-dimensional scatter plot like "stripchart", but with closely-packed, non-overlapping points. This packages provides a wrapper around the original beeswarm package written by Aron Eklund. This means that the original functions are not modified in any way or form.

URL: http://www.cbs.dtu.dk/~eklund/beeswarm/

### Install 

To install beeplotR from Github requires the devtools package from CRAN. That means running the following commands:

#### Windows:
```
source("https://rawgit.com/greenore/initR/master/init.R")
packagesGithub("beeplotR", repo_name="greenore")
```

#### Linux:
```
source(pipe(paste("wget -O -", "https://rawgit.com/greenore/initR/master/init.R")))
packagesGithub("beeplotR", repo_name="greenore")
```
