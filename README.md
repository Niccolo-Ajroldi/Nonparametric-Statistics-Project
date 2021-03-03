# Nonparametric analysis of 2020 US presidential elections

This is the repository for the project of *Nonparametric Statistics* course held at Politecnico di Milano during academic year 2020/2021. The aim of this work is to give an insight on 2020 US presidential election, in particular in understanding which factors may have infuenced the election.

A first part of the analysis is devoted to making some preliminary inference on different socio-economic and demographic factors at our disposal. We then focus on the fitting of a GAM prediction model using a part of our data as training set. We will compare it with respect to other possible models through a validation set, and, in the end, we will evaluate its predictive performances through the remaining observations, used as test set.

## Repository structure
The code is strucured as follows:
- `/data`: folder containing all the data used for the analysis
- `/code`: folder containing the codes for the analysis, further divided into:
    - `/data clean`: codes for cleaning and merging data-sets
    - `/Inference`: codes for the Inference part, on county level
    - `/Model`: codes for prediction part: GAM, SVM, Random Forest
    - `/Nations`: codes for the Inference part, on state level
    - `install.R`: file that provides automatic installation of required R packages
- `/Pics`: folder containing most of the pics present in the report
- `Ajroldi-Lurani-Marchionni.pdf`: report for the analysis

### Installation
The file `install.R` provides automatic installation of the required packages.

### References
Computation were performed using 
>  R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

For all the R-packages used in the codes please refer to section Reference in the [report](https://github.com/Niccolo-Ajroldi/Nonparametric-Statistics-Project/blob/master/NPS-Report-Ajroldi-Lurani-Marchionni.pdf).



## Authors
Niccolò Ajroldi - Politecnico di Milano  \
Agostino Lurani - Politecnico di Milano \
Edoardo Marchionni - Politecnico di Milano \
Master of Science in Mathematical Engineering students at Politecnico di Milano.
