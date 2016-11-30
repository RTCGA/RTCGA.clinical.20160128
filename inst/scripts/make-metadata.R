meta <- data.frame(
    Title = rep(" Clinical datasets from The Cancer Genome Atlas Project from 2016-01-28 ", 34 ),
    Description = rep(" Package provides clinical datasets from The Cancer Genome Atlas
Project for all cohorts types from http://gdac.broadinstitute.org/. Clinical
data format is explained on NCI TCGA wiki https://wiki.nci.nih.gov/display/TCGA/
Clinical+Data+Overview. Data taken from 2016-01-28 release. All release dates are
available here http://gdac.broadinstitute.org/runs/ . ", 34 ),
    BiocVersion = rep("3.4", 34 ),
    SourceUrl = "http://gdac.broadinstitute.org/",
    SourceVersion =  "2016-01-28" ,
    DataProvided = "TCGA",
    Maintainer = "Bioconductor Package Maintainer <maintainer@bioconductor.org>",
    RDataClass = rep("data.frame", 34 ),
    ResourceName =  c("ACC.clinical.20160128","BLCA.clinical.20160128","BRCA.clinical.20160128","CESC.clinical.20160128","CHOL.clinical.20160128","COADREAD.clinical.20160128","DLBC.clinical.20160128","ESCA.clinical.20160128","FPPP.clinical.20160128","GBMLGG.clinical.20160128","HNSC.clinical.20160128","KICH.clinical.20160128","KIPAN.clinical.20160128","KIRC.clinical.20160128","KIRP.clinical.20160128","LAML.clinical.20160128","LIHC.clinical.20160128","LUAD.clinical.20160128","LUSC.clinical.20160128","MESO.clinical.20160128","OV.clinical.20160128","PAAD.clinical.20160128","PCPG.clinical.20160128","PRAD.clinical.20160128","SARC.clinical.20160128","SKCM.clinical.20160128","STAD.clinical.20160128","STES.clinical.20160128","TGCT.clinical.20160128","THCA.clinical.20160128","THYM.clinical.20160128","UCEC.clinical.20160128","UCS.clinical.20160128","UVM.clinical.20160128") )
write.csv(meta, file = "inst/extdata/metadata.csv", row.names = FALSE)
