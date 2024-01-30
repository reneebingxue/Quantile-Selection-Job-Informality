********PSE - MICROECONOMETRICS
***INFORMAL EARNINGS GAPS IN COLOMBIA: A QUINTILE REGRESSION APPROACH WITH SAMPLE SELECTION CORRECTION
***LI & SANCHEZ-TORRES
**************--------------------------------------------------------------------------------------------------********LOAD AND SELECT THE DATA NEEDED
// Load the data
use "/Users/renee/Desktop/Micrometrics/GEIH/GEIH_2022_S14.dta", clear
use "/Users/renee/Desktop/Micrometrics/GEIH/data_june.dta", clear
save merged_data.dta, replace
// Renaming variables for better understanding
rename P6040 age
rename P3271 gender
rename PET working_age
rename DSI unemployed
rename DSCY unemployed_sr
rename ocu employed
rename FFT out_labor
rename INGLABO monthly_income
rename P6800 weekly_whours
rename CLASE urban_rural
rename DPTO province
rename P6080 ethnicity
rename P6070 marital_status
rename Discapacidad disability
rename P6426 seniority
rename P6440 contracted
rename P6450 verbal_written
rename P6430 worker_type
rename P6920 pension_fund
// Keep selected variables for analysis
keep DIRECTORIO SECUENCIA_P ORDEN AREA informal sector nolabor_income members ///
     children weight cities age gender working_age educ_years unemployed ///
     employed out_labor unemployed_sr ///
     monthly_income weekly_whours urban_rural province ///
     ethnicity marital_status disability seniority ///
     contracted verbal_written worker_type pension_fund ///
	  hlabincome  linglabo  lhlabincome  educ_years2_  head_hh selection lnolabor_income
	  
******COMPUTATION OF REQUIRED VARIABLES
// Calculate squared terms
gen age2 = age^2
gen labincomen_0 = (hlabincome != 0)
replace labincomen_0 = . if missing(hlabincome)
encode AREA, gen(domin)
gen couple= 1 if marital_status<4
replace couple=0 if couple==. & marital_status!=.
tabulate head_hh, generate(head_hh)
tabulate sector, generate(sector)
tabulate domin, generate(domin)
gen d_nolabor_income=1 if nolabor_income>0 & nolabor_income!=.
replace d_nolabor_income=0 if d_nolabor_income==.
replace lnolabor_income =0 if d_nolabor_income==0
gen inter_nli=d_nolabor_income*lnolabor_income
drop selection 
gen selection=0 if working_age==1 & employed!=1 & (age>15 & age<66)
replace selection=1 if employed==1 & (lhlabincome>0 & lhlabincome!=.) & (age>15 & age<66)
// Keep observations whose age is between 16 and 65, of working age
// Exclude observations where citie equals 0
// Exclude agricultural population?
gen sample=1 if employed==1 & (age>15 & age<66) & (lhlabincome>0 & lhlabincome!=.) & cities==1
replace sample=0 if sample==.
gen sample2=1 if (age>15 & age<66) & cities==1
replace sample2=0 if sample2==.

// Save the final selected data
save filtered_data.dta, replace

***************DESCRIPTIVES STATISTICS
***Rate of informality
sum  informal [w=weight] if cities==1 // 46%
sum informal [w=weight] if cities==1 & worker_type==1 //24%
***Average income
bysort informal: sum monthly_income [w=weight] if cities==1 // 2.420.600 - 1.108.386
bysort informal: sum monthly_income [w=weight] if cities==1 & worker_type==1 // 2.156.828 - 1.269.477
bysort informal: sum hlabincome [w=weight] if cities==1 // 12.576 - 6.561
bysort informal: sum hlabincome [w=weight] if cities==1 & worker_type==1 // 10.916 - 6.787
***Formal&Informal Group Comparison 


******PLOT KERNEL DENSITIES AND WAGE DIFFERENTIALS ACROSS QUANTILES. 
replace weight=weight*12
recast int weight, force
kdensity lhlabincome [w=weight] if informal==1 & lhlabincome>6 & lhlabincome<12, addplot(kdensity lhlabincome [w=weight] if informal==0 & lhlabincome>6 & lhlabincome<12)
twoway kdensity lhlabincome [w=weight] if informal==1 &  lhlabincome>6 & lhlabincome<12 ||  kdensity lhlabincome [w=weight] if informal==0 & lhlabincome>6 & lhlabincome<12
kdensity lhlabincome [w=weight] if worker_type==1 & informal==1 & lhlabincome>6 & lhlabincome<12, addplot(kdensity lhlabincome [w=weight] if worker_type==1 & informal==0 & lhlabincome>6 & lhlabincome<12)
kdensity lhlabincome [w=weight] if  worker_type!=1 & informal==1 & lhlabincome>6 & lhlabincome<12, addplot(kdensity lhlabincome [w=weight] if worker_type!=1 & informal==0 & lhlabincome>6 & lhlabincome<12)

twoway ///
    (kdensity lhlabincome if informal == 0 & worker_type == 1 [w = weight], ///
    color(blue) legend(order(1 "Formal" 2 "Informal")) title("Formal Workers")) ///
    || ///
    (kdensity lhlabincome if informal == 1 & worker_type == 1 [w = weight], ///
    color(red) legend(order(1 "Formal" 2 "Informal")) title("Informal Workers")), ///
    xtitle("Private Sector Employees")
graph save "graph_1.gph", replace
twoway ///
    (kdensity lhlabincome if informal == 0 & worker_type != 1 [w = weight], ///
    color(blue) legend(order(1 "Formal" 2 "Informal")) title("Formal Workers")) ///
    || ///
    (kdensity lhlabincome if informal == 1 & worker_type != 1 [w = weight], ///
    color(red) legend(order(1 "Formal" 2 "Informal")) title("Informal Workers")), ///
    xtitle("Other Worker Types")
graph save "graph_2.gph", replace
graph combine graph_1.gph graph_2.gph, title("Kernel Density of Wage For Formal and Informal Workers")
graph save `kds_full',replace

*******Baseline Regressions
*1. Mincer OLS Regression
reg lhlabincome informal if sample2==1
reg lhlabincome informal gender educ_years educ_years2 age marital_status i.head_hh if sample2==1
reg lhlabincome informal gender educ_years educ_years2 age marital_status i.head_hh seniority  i.sector i.domin  if sample2==1
*2. Unobserved Outcome for selected group
gen unobsinc = .
replace unobsinc = 1 if employed == 1 & missing(hlabincome)
replace unobsinc = 0 if employed == 0 | !missing(hlabincome)
*3. Run a Probit model of missing and covariates of interest. 
probit unobsinc informal gender educ_years educ_years2_ age i.head_hh i.sector 
*Because we find significant selection here, we are going to update our selection variable

*4. Heckmand Selection Model 
heckman lhlabincome informal gender educ_years educ_years2 age couple i.head_hh seniority  i.sector i.domin  if sample2==1, ///
	select(selection = gender educ_years age couple i.head_hh children d_nolabor_income inter_nli i.domin) twostep
	
*Q-Q plot of residuals

****INCLUDING INTERACTION WITH OCCUPATIONAL TYPE (NON WAGE-EARNING WORKERS)
*** f: formal; i: informal; wew: wage-earning workers; nwew: non wage-earning workers
*** Reference group: formal and wage-earning workers
*** inw: informal and non wage-earning workers
*** fnw
gen wew=1 if worker_type==1 | worker_type==2
replace wew=0 if wew==. & informal!=.
gen nwew=1-wew 
gen formal=1-informal
gen inw=informal*nwew
gen fnw=formal*nwew


*******Baseline Quantile Regressions
use "/Users/renee/Desktop/Micrometrics/GEIH/GEIH_2022_S14_expanded.dta", clear
qreg lhlabincome informal gender educ_years educ_years2 age couple head_hh2  head_hh3 seniority i.sector domin if sample2==1 [pweight = weight]
sqreg lhlabincome informal gender educ_years educ_years2 age couple head_hh2  head_hh3 seniority   sector2 sector3 sector4 sector5 sector6 sector7 sector8 domin if sample2==1, q(0.1 0.25 0.5 0.75 0.9)
save data_june.dta, replace

Estimation using AB method
use "/Users/renee/Desktop/Micrometrics/GEIH/data_june.dta", clear
ssc install qregsel
gen sel=0
replace sel=1 if lhlabincome!=.
**Estimation using AB Method
qregsel lhlabincome informal gender educ_years educ_years2 age seniority  if sample2==1, ///
	select(selection = gender educ_years age couple children d_nolabor_income inter_nli) quantile (0.1 0.5 0.9)
ereturn list
svmat e(grid), name(col)
qui gen lvalue = log10(value)
twoway connected lvalue spearman
** Prediction
set seed 1
predict wage_hat participation
_pctile wage_hat, nq(20)
mat qs = J(19,3,.)
forvalues i=1/19 {
	mat qs[`i',1] = r(r`i')
}
_pctile wage, nq(20)
forvalues i=1/19 {
	mat qs[`i',2] = r(r`i')
	mat qs[`i',3] = `i'
}
svmat qs, name(quantiles)
twoway connected quantiles1 quantiles2 quantiles3, ///
 xtitle("Ventile") ytitle("Wage") legend(order(1 "Corrected" 2 "Uncorrected"))
** Inference
bootstrap rho=e(rho) _b, reps(100) seed(2) notable: lhlabincome informal gender educ_years educ_years2 age seniority if sample2==1, ///
	select(selection = gender educ_years age couple children d_nolabor_income inter_nli) quantile(0.1 0.5 0.9)  
estat bootstrap, percentile



** Load data and estimate quantile regression 
webuse womenwk,clear

** Estimation using AB method
global wage_eqn wage educ age
global seleqn married children educ age
qregsel $wage_eqn, select($seleqn) quantile(.1 .5 .9) 
ereturn list
svmat e(grid), name(col)
qui gen lvalue = log10(value)
twoway connected lvalue spearman

** Prediction
set seed 1
predict wage_hat participation
_pctile wage_hat, nq(20)
mat qs = J(19,3,.)
forvalues i=1/19 {
	mat qs[`i',1] = r(r`i')
}
_pctile wage, nq(20)
forvalues i=1/19 {
	mat qs[`i',2] = r(r`i')
	mat qs[`i',3] = `i'
}
svmat qs, name(quantiles)
twoway connected quantiles1 quantiles2 quantiles3, ///
 xtitle("Ventile") ytitle("Wage") legend(order(1 "Corrected" 2 "Uncorrected"))

** Inference
bootstrap rho=e(rho) _b, reps(100) seed(2) notable: qregsel $wage_eqn, ///
	select($seleqn) quantile(.1 .5 .9)  
estat bootstrap, percentile
