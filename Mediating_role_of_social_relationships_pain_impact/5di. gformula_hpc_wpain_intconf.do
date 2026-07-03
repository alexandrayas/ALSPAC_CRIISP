**********************************************************************************
/* 

Mediation analysis using gformula by Monto Carlo simulations, adjusting for baseline and early-life confounders.

SUBSET TO THOSE WITH PAIN AT BASELINE, WITH INTERMEDIATE CONFOUNDERS

*/

**********************************************************************************
*Have to run due to error on HPC/BC4: command ChkIn is unrecognized r(199)
do "/user/home/username/ado/plus/c/chkin.ado"

*Set args 
args modn

*Start logging with a unique log file for each iteration
log using "gformula_wpain_intc_log_`modn'.log", replace

*Print impuation interaction number
di "Model number is:  " `modn'

*Load and filter the data
use "ldat_intconf_pain.dta", clear
di "`modn'"
keep if mod_n == `modn'


*Define variables
local baselineconflist "sex ethnicity m_pregsmk parent_sc_0 m_homown_0 parent_mhp_1"
local baselineconflist2 "sex ethnicity m_pregsmk parent_sc_0 m_homown_0 parent_mhp_1"


*Run mediation model
gformula outcome exposure mediator intconf `baselineconflist2', ///
mediation outcome(outcome) exposure(exposure) mediator(mediator) ///
base_confs(`baselineconflist2') ///
post_confs(intconf) ///
obe control(mediator:1)  ///
commands(intconf:logit, mediator:logit, outcome:logit) ///
equations(outcome: exposure mediator intconf `baselineconflist', mediator: exposure intconf `baselineconflist', intconf: exposure `baselineconflist') ///
samples(1000) moreMC simulations(10000) logOR minsim replace seed(79)

*Return results
return list

*End logging
log close