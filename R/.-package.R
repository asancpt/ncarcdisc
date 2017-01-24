

#' Noncompartmental Analysis for Pharmacokinetic Data
#' 
#' It conducts a noncompartmental analysis(NCA) as closely as possible to the
#' most widely used commercial pharmacokinetic analysis software.
#' 
#' The main functions are \preformatted{ NCA to perform NCA for many subjects.
#' 
#' IndiNCA to perform NCA for one subject. }
#' 
#' @name NonCompart-package
#' @aliases NonCompart-package NonCompart
#' @docType package
#' @section Acknowledgement: Author thanks for the careful review and valuable
#' input of Dr. Jee Eun Lee.
#' @author Kyun-Seop Bae <k@@acr.kr>
#' @references \enumerate{ \item Gabrielsson J, Weiner D. Pharmacokinetic and
#' Pharmacodynamic Data Analysis - Concepts and Applications. 5th ed. 2016.
#' \item Shargel L, Yu A. Applied Biopharmaceutics and Pharmacokinetics. 7th
#' ed. 2015.  \item Rowland M, Tozer TN. Clinical Pharmacokinetics and
#' Pharmacodynamics - Concepts and Applications. 4th ed. 2011. \item Gibaldi M,
#' Perrier D. Pharmacokinetics. 2nd ed. revised and expanded. 1982. }
#' @keywords package NCA
#' @examples
#' 
#' # Theoph and Indometh data: dose in mg, conc in mg/L, time in h
#' NCA(Theoph, "Subject", "Time", "conc", Dose=320)
#' NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus")
#' 
#' iAUC = data.frame(Name=c("AUC[0-12h]","AUC[0-24h]"), Start=c(0,0), End=c(12,24)) ; iAUC
#' NCA(Theoph, "Subject", "Time", "conc", Dose=320, iAUC=iAUC)
#' NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus", iAUC=iAUC)
#' 
#' writeLines(NCA(Theoph, "Subject", "Time", "conc", Dose=320, Report="Text"),
#'            "Theoph_Linear_CoreOutput.txt")
#' writeLines(NCA(Theoph, "Subject", "Time", "conc", Dose=320, Method="Log", Report="Text"),
#'            "Theoph_Log_CoreOutput.txt")
#' writeLines(NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus", Report="Text"),
#'            "Indometh_Bolus_Linear_CoreOutput.txt")
#' writeLines(NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Bolus", Method="Log",
#'            Report="Text"), "Indometh_Bolus_Log_CoreOutput.txt")
#' writeLines(NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Infusion", TimeInfusion=0.25,
#'            Report="Text"), "Indometh_Infusion_Linear_CoreOutput.txt")
#' writeLines(NCA(Indometh, "Subject", "time", "conc", Dose=25, AdmMode="Infusion", TimeInfusion=0.25,
#'            Method="Log", Report="Text"), "Indometh_Infusion_Log_CoreOutput.txt")
#' 
#' IndiNCA(Theoph[Theoph$Subject==1,"Time"], Theoph[Theoph$Subject==1, "conc"], Dose=320)
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Bolus")
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Infusion", TimeInfusion=0.25)
#' 
#' IndiNCA(Theoph[Theoph$Subject==1,"Time"], Theoph[Theoph$Subject==1, "conc"], Dose=320,
#'         Report="Text")
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Bolus", Report="Text")
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Infusion", TimeInfusion=0.25, Report="Text")
#' 
#' iAUC = data.frame(Name=c("AUC[0-12h]","AUC[0-24h]"), Start=c(0,0), End=c(12,24)) ; iAUC
#' IndiNCA(Theoph[Theoph$Subject==1,"Time"], Theoph[Theoph$Subject==1, "conc"], Dose=320,
#'         iAUC=iAUC)
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Bolus", iAUC=iAUC)
#' IndiNCA(Indometh[Indometh$Subject==1,"time"], Indometh[Indometh$Subject==1, "conc"], Dose=25,
#'         AdmMode="Infusion", TimeInfusion=0.25, iAUC=iAUC)
#' 
NULL





#' NCA Report Configuation Table
#' 
#' Contains the names and order of colum of return table/text by \code{IndiNCA}
#' and \code{NCA} functions
#' 
#' This table should exist in NonCompart package. User can edit this table for
#' shaping the report in one's own style.
#' 
#' @name RptCfg
#' @docType data
#' @format A data frame with 48 observations on the following 10 variables.
#' \describe{ \item{list("PPTESTCD")}{a character vector of CDISC SDTM
#' PPTESTCD} \item{list("SYNONYM")}{a character vector of CDISC SDTM PPTESTCD
#' Synonym} \item{list("NCI")}{a character vector of NCI peferred terms}
#' \item{list("WNL")}{a character vector of WinNonlin(R) software variables}
#' \item{list("ExtravascularDefault")}{a numeric vector of ordering in report
#' for extravascular administration, Zero means exclusion in the report.}
#' \item{list("ExtravascularWNL")}{a numeric vector of WinNonlin(R) style
#' ordering in report for extravascular administration, Zero means exclusion in
#' the report.} \item{list("BolusDefault")}{a numeric vector of ordering in
#' report for extravascular administration, Zero means exclusion in the
#' report.} \item{list("BolusWNL")}{a numeric vector of WinNonlin(R) style
#' ordering in report for extravascular administration, Zero means exclusion in
#' the report.} \item{list("InfusionDefault")}{a numeric vector of ordering in
#' report for extravascular administration, Zero means exclusion in the
#' report.} \item{list("InfusionWNL")}{a numeric vector of WinNonlin(R) style
#' ordering in report for extravascular administration, Zero means exclusion in
#' the report.} }
#' @keywords datasets
NULL



