**********************************************************************************
/* 

Mediation analysis using gformula by Monto Carlo simulations, adjusting for baseline and early-life confounders.

SUBSET TO THOSE WITH PAIN AT BASELINE, NO INTERMEDIATE CONFOUNDERS

*/

**********************************************************************************
*Have to run due to error on HPC/BC4: command ChkIn is unrecognized r(199)
do "/user/home/username/ado/plus/c/chkin.ado"

*Set args 
args modn

*Start logging with a unique log file for each iteration
log using "gformula_wpain_log_`modn'.log", replace

*Print impuation interaction number
di "Model number is:  " `modn'

*Load and filter the data
use "ldat_pain.dta", clear
di "`modn'"
keep if mod_n == `modn'


*Define variables
local baselineconflist "sex ethnicity m_pregsmk parent_sc_0 m_homown_0 parent_mhp_1"
local baselineconflist2 "sex ethnicity m_pregsmk parent_sc_0 m_homown_0 parent_mhp_1"


*Run mediation model
gformula outcome exposure mediator `baselineconflist2', ///
mediation outcome(outcome) exposure(exposure) mediator(mediator) ///
base_confs(`baselineconflist2') ///
obe control(mediator:1)  ///
commands(mediator:logit, outcome:logit) ///
equations(outcome: exposure mediator `baselineconflist', mediator: exposure `baselineconflist') ///
samples(1000) moreMC simulations(10000) logOR minsim replace seed(79)
		
*Return results
return list

*End logging
log close