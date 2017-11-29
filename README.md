# ncarcdisc: Noncompartmental Analysis of Pharmacokinetic Data for CDISC Standardized Parameters 

> ncarcdisc R package (Branch of NonCompart v0.2.6 R package)

Overview
--------

Description: Conduct a noncompartmental analysis as closely as possible to the most widely used commercial software for pharmacokinetic analysis, i.e. 'Phoenix(R) WinNonlin(R)' <https://www.certara.com/software/pkpd-modeling-and-simulation/phoenix-winnonlin/>.

Some features include:

1. Use CDISC SDTM PP domain terms.
2. Automatic slope selection with the same criterion of WinNonlin(R)
3. Support both 'linear-up linear-down' and 'linear-up log-down' method
4. Calculate partial(interval) AUC with 'linear' or 'log' interpolation method
5. Perform a noncompartmental analysis of CDISC standardized pharmacokinetic dataset (.XPT)

For more details on noncompartmental analysis, see the reference: Gabrielsson J, Weiner D. Pharmacokinetic and Pharmacodynamic Data Analysis - Concepts and Applications. 5th ed. 2016. (ISBN:9198299107)

> Acknowledgement: Author thanks for the careful review and valuable input of Dr. Jee Eun Lee.

Installation
------------

#### CRAN

    # get stable version
    install.packages('ncar')

#### Github

    # get development version
    install.packages('devtools')
    devtools::install_github('asancpt/ncar')
   
Contact
-------

- `ncar` Package developer: Kyun-Seop Bae <k@acr.kr>, Sungpil Han <shan@acp.kr>
- Copyright: 2017, Kyun-Seop Bae
- License: GPL-3

