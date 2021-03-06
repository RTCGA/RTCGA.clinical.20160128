# RTCGA.clinical.20160128 

This package was created with [`RTCGA::createTCGA()`](http://rtcga.github.io/RTCGA/staticdocs/createTCGA.html) function and is a part of [RTCGA](http://rtcga.github.io/RTCGA/) project. It consist of
data from [The Cancer Genome Atlas Project](https://cancergenome.nih.gov/abouttcga). 

Datasets existing in this package were downloaded automatically from [Firehose Broad GDAC](http://gdac.broadinstitute.org/) portal. They were taken
from the 2016-01-28 release date. All release dates are available [here](http://gdac.broadinstitute.org/runs/). Datasets were downloaded with the use of `RTCGA::downloadTCGA()` function and were transposed with `RTCGA::readTCGA()` function.

The package contains following datasets, which names corresponds to: the cohort type, data type and release date. Cohort types can be checked
with `RTCGA::infoTCGA()`, release dates with `RTCGA::checkTCGA('Dates')` and data types with e.g. `RTCGA::checkTCGA('DataSets', 'BRCA')` calls. 
The used data type for this package was ` Merge_Clinical.Level_1 ` - all those information are included in the `DESCRIPTION` file. To see
the manual page for included datasets run  `?clinical.20160128` in R console. 

-  ACC.clinical.20160128 
-  BLCA.clinical.20160128 
-  BRCA.clinical.20160128 
-  CESC.clinical.20160128 
-  CHOL.clinical.20160128 
-  COADREAD.clinical.20160128 
-  DLBC.clinical.20160128 
-  ESCA.clinical.20160128 
-  FPPP.clinical.20160128 
-  GBMLGG.clinical.20160128 
-  HNSC.clinical.20160128 
-  KICH.clinical.20160128 
-  KIPAN.clinical.20160128 
-  KIRC.clinical.20160128 
-  KIRP.clinical.20160128 
-  LAML.clinical.20160128 
-  LIHC.clinical.20160128 
-  LUAD.clinical.20160128 
-  LUSC.clinical.20160128 
-  MESO.clinical.20160128 
-  OV.clinical.20160128 
-  PAAD.clinical.20160128 
-  PCPG.clinical.20160128 
-  PRAD.clinical.20160128 
-  SARC.clinical.20160128 
-  SKCM.clinical.20160128 
-  STAD.clinical.20160128 
-  STES.clinical.20160128 
-  TGCT.clinical.20160128 
-  THCA.clinical.20160128 
-  THYM.clinical.20160128 
-  UCEC.clinical.20160128 
-  UCS.clinical.20160128 
-  UVM.clinical.20160128 

Optionally, the data can be loaded through the [ExperimentHub](http://www.bioconductor.org/packages/3.4/bioc/vignettes/ExperimentHubData/inst/doc/ExperimentHubData.html) interface.

```{r, eval=FALSE}
library(ExperimentHub)
eh <- ExperimentHub()
myfiles <- query(eh,  "RTCGA.clinical.20160128" )
myfiles[[1]]  ## load the first resource in the list
```


# Installation 

To install this package from GitHub use
```{r, eval=FALSE}
library(RTCGA) 
 installTCGA("RTCGA.clinical.20160128") 
```

Make sure you have [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed on your computer, if you are trying devtools on Windows.

# Notes

Note that this package is a data package with datasets from 2016-01-28 release date. There are few data packages already on Bioconductor with datasets from "2015-11-01". To read more check `?RTCGA::datasetsTCGA`.