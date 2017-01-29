-
# Report of *ncappc* package [*]
-
------------------------------------------------------------------

==============================================================



-

Summary of the data set and the results
-------------------------------



```
## Name of the file with the observed data: "Theoph.csv"
## Route of administration: extravascular
## Dose type: non-steady-state
## No. of population stratification level: 0
```

-


| Dose (M) | No. of individuals |
|:--------:|:------------------:|
|    NA    |         12         |

-------------------------------
-
Command-line arguments passed to ncappc function
-------------------------------


```
## ncappc(obsFile = "Theoph.csv", psnOut = FALSE, method = "linear-log", 
##     evid = FALSE, noPlot = TRUE, printOut = TRUE)
```


------------------------------
-
Description of the tabular output
------------------------------

**Table 1 (ncaOutput.tsv).** The ***ncappc*** functionality produces this table to report the estimated values of the NCA metrics described in the documentation for each individual along with other stratifiers (eg. population group ID, dose ID, etc.) if specified in the input command. The extension "tsv" stands for "tab separated variable", *i.e.*, the columns in this table are separated by tabs. "NaN" or "NA" is produced for the NCA metrics which are irrelevant for the specified data type. Below is an excerpt of selected columns of top 100 rows.

# Table 1. ncaOutput.tsv (selected columns of top 100 rows)

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Thu Jan 26 16:56:31 2017 -->
<table border=1>
<tr> <th> ID </th> <th> Dose
(M) </th> <th> AUClast
(T*M.L^-3) </th> <th> Cmax
(M.L^-3) </th> <th> Tmax
(T) </th> <th> AUCINF_obs
(T*M.L^-3) </th> <th> Vz_obs
(M/M.L^-3) </th> <th> Cl_obs
(M/(T*M.L^-3)) </th> <th> HL_Lambda_z
(T) </th>  </tr>
  <tr> <td align="center"> 1 </td> <td align="center"> NA </td> <td align="center"> 147.2347 </td> <td align="center"> 10.5000 </td> <td align="center"> 1.1200 </td> <td align="center"> 214.9236 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center"> 14.3044 </td> </tr>
  <tr> <td align="center"> 2 </td> <td align="center"> NA </td> <td align="center">  88.7313 </td> <td align="center">  8.3300 </td> <td align="center"> 1.9200 </td> <td align="center">  97.3779 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  6.6593 </td> </tr>
  <tr> <td align="center"> 3 </td> <td align="center"> NA </td> <td align="center">  95.8782 </td> <td align="center">  8.2000 </td> <td align="center"> 1.0200 </td> <td align="center"> 106.1277 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  6.7661 </td> </tr>
  <tr> <td align="center"> 4 </td> <td align="center"> NA </td> <td align="center"> 102.6336 </td> <td align="center">  8.6000 </td> <td align="center"> 1.0700 </td> <td align="center"> 114.2162 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  6.9812 </td> </tr>
  <tr> <td align="center"> 5 </td> <td align="center"> NA </td> <td align="center"> 118.1794 </td> <td align="center"> 11.4000 </td> <td align="center"> 1.0000 </td> <td align="center"> 136.3047 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  8.0023 </td> </tr>
  <tr> <td align="center"> 6 </td> <td align="center"> NA </td> <td align="center">  71.6970 </td> <td align="center">  6.4400 </td> <td align="center"> 1.1500 </td> <td align="center">  82.1759 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  7.8950 </td> </tr>
  <tr> <td align="center"> 7 </td> <td align="center"> NA </td> <td align="center">  87.9692 </td> <td align="center">  7.0900 </td> <td align="center"> 3.4800 </td> <td align="center"> 100.9876 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  7.8467 </td> </tr>
  <tr> <td align="center"> 8 </td> <td align="center"> NA </td> <td align="center">  86.8066 </td> <td align="center">  7.5600 </td> <td align="center"> 2.0200 </td> <td align="center"> 102.1533 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  8.5100 </td> </tr>
  <tr> <td align="center"> 9 </td> <td align="center"> NA </td> <td align="center">  83.9374 </td> <td align="center">  9.0300 </td> <td align="center"> 0.6300 </td> <td align="center">  97.5200 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  8.4060 </td> </tr>
  <tr> <td align="center"> 10 </td> <td align="center"> NA </td> <td align="center"> 135.5761 </td> <td align="center"> 10.2100 </td> <td align="center"> 3.5500 </td> <td align="center"> 167.8600 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  9.2469 </td> </tr>
  <tr> <td align="center"> 11 </td> <td align="center"> NA </td> <td align="center">  77.8935 </td> <td align="center">  8.0000 </td> <td align="center"> 0.9800 </td> <td align="center">  86.9026 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  7.2612 </td> </tr>
  <tr> <td align="center"> 12 </td> <td align="center"> NA </td> <td align="center"> 115.2202 </td> <td align="center">  9.7500 </td> <td align="center"> 3.5200 </td> <td align="center"> 125.8315 </td> <td align="center"> NA </td> <td align="center"> NA </td> <td align="center">  6.2865 </td> </tr>
   </table>

**Table 2 (Obs_Stat.tsv).** A set of statistical parameters calculated for the entire population or the stratified population for the following NCA metrics estimated from the observed data: Tmax, Cmax, AUClast, AUClower_upper, AUCINF_obs, AUC_pExtrap_obs, AUCINF_pred, AUC_pExtrap_pred, AUMClast, AUMCINF_obs, AUMC_pExtrap_obs, AUMCINF_pred, AUMC_pExtrap_pred, HL_Lambda_z, Rsq, Rsq_adjusted, No_points_Lambda_z obtained from the observed data. Brief description of the calculated statistical parameters: **Ntot** = Total number of data points, **Nunique** = number of unique data points, **Min** = minimum value, **Max** = maximum value, **Mean** = mean/average value, **SD** = standard deviation, **SE** = standard error, **CVp** = coefficient of variation %, **a95CIu** = upper limit of 95% arithmetic confidence interval, **a95CIl** = lower limit of 95% arithmetic confidence interval, **gMean** = geometric mean, **gCVp** = geometric coefficient of variation %.

-
---------------------------------
Description of the graphical output
------------------------------


```
## [1] "No concentration vs time plot is available."
```

**Figure 1.** [Individual level] Concentration vs time profile for each individual stratified by dose or population group, if any, as obtained from the observed data. The left panels represent the raw data, while the right panels represent the semi-logarithmic form of the concentration data. Each of the lines represents individual data.

-


```
## [1] "No histogram is available as the number of individuals is less than 5 in each population strata!"
```

**Figure 2.** [Population level] Histogram of four selected NCA metrics (AUClast, AUCINF_obs, Cmax, Tmax) estimated from the observed data. The solid blue vertical and dotted lines represent the population mean and the "spread" of the data. The "spread" is defined by 95% nonparametric prediction interval of the NCA metrics obtained from the observed data.



-
[*] Created and maintained by:
### Chayan Acharya, Andrew C. Hooker, Siv Jonsson, Mats O. Karlsson
### *Department of Pharmaceutical Biosciences*
### *Uppsala University, Sweden*

