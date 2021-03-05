// This code create the Tables in the main text and appendix
clear
clear mata
clear matrix
set more  off
set graph off
set maxvar  32767
set matsize 11000
set emptycells drop
cd  "/home/aneko/OUI/DF2/RR2/"
{ //Key variables
use "Data_public.dta", clear
	g age_p1 = age^1
	g age_p2 = age^2
	g age_p3 = age^3
	g age_p4 = age^4
	g age40 = age>=40
	g age_q1 = ((age-40)^1)*age40
	g age_q2 = ((age-40)^2)*age40
	g age_q3 = ((age-40)^3)*age40
	g age_q4 = ((age-40)^4)*age40
	g lwage0    = ln(monthly_wage_0)
	g lwagen0   = ln(monthly_wage_n0) 
	g ay1	    = ned if ned<2*365
	g ay2       = ned<=30*7
	g ay3       = ned<=39*7
	g ay4       = wg_c if ned<2*365
	g ay5       = lwagen0               if ned<2*365 & wv
	g ay6       = lwagen0>lwage0+ln(.5) if ned<2*365 & wv & !mi(lwagen0) & !mi(lwage0)
tempfile A_0	
save    `A_0', replace
}	

{ //global var lists
gl varp1 age_p1				  			age_q1	
gl varp2 age_p1	age_p2			  		age_q1	age_q2	
gl varp3 age_p1	age_p2	age_p3		  	age_q1	age_q2	age_q3	
gl varp4 age_p1	age_p2	age_p3	age_p4  age_q1	age_q2	age_q3	age_q4
gl cova  i.end_w  // many covariates are not included in the public data, so the estimates with controls are not replicable
}
{ //Tables
{ //T1
local tn = "1"
use          "Data_I.dta", clear
g ii_agri = industry==1
g ii_manu = industry==2
g ii_cons = industry==3
g ii_serv = industry!=1 & industry!=2 & industry!=3 
g s1 = 1
g s2 =  clay==1
g s3 = (clay==1) & (work10>6*365) & (work5 >3*365) & (age>30 & age<50)
g s5 = (clay==1) & (work10>6*365) & (work5 >3*365) & (age>30 & age<50) & (end >= mdy(8,1,1989))
replace recall = . if recall==2
recode educ (0=.)(1=0)(2/max=1), g(morecomp)
g lud = ned<7*30
g llud         = ned<2*365
g num_obs = 1 
format num_ %9.0f
//replace tenure = tenure/7
replace work2  = work2/(2*365)
replace work5  = work5/(5*365)
replace work10  = work10/(10*365)
replace ned    = ned     if ned<2*365  // 7
replace ned    = .     	 if ned>=2*365
replace wg_c   = .    	 if ned>=2*365
g malefrac = mfirmsize/firmsize
g wg_cr  = wg_c     if recall==1
g wg_cn  = wg_c     if recall==0
g n_tenurer  = n_tenure if recall==1
g n_tenuren  = n_tenure if recall==0
keep female married austrian age morecomp blue tenure work2 work5 monthly_wage_0 ///
	firmsize malefrac workerage meanWage ///
	ned lud llud wg_c  exp_r recall n_tenure work10 ///
	n_tenuren n_tenurer wg_cn wg_cr ii_a ii_m ii_c ii_s clay s3 s5 num_obs
gl varcare female married austrian age morecomp blue tenure work2 work5 monthly_wage_0 ///
	firmsize malefrac workerage meanWage ///
	ned lud llud wg_c  exp_r recall n_tenure work10 ///
	n_tenuren n_tenurer wg_cn wg_cr ii_a ii_m ii_c ii_s 	
count
{
preserve
g name    = "1total"
collapse (count) num_obs (mean) $varcare, by(name)
}
tempfile 01
save    `01'
{
restore
preserve
g name    = "1total"
collapse (count) num_obs (sd) $varcare, by(name)
replace name = name  + "SD"
}
append using `01'
tempfile 02
save    `02'
{
restore
preserve
keep if clay==1
g name    = "2laidoff"
collapse (count) num_obs (mean) $varcare, by(name)
}
append using `02'
tempfile 03
save    `03'
{
restore
preserve
keep if clay==1
g name    = "2laidoffsd"
collapse (count) num_obs (sd) $varcare, by(name)
}
append using `03'
tempfile 04
save    `04'
{
restore
preserve
keep if s3==1
g name    = "3sample"
collapse (count) num_obs (mean) $varcare, by(name)
}
append using `04'
tempfile 05
save    `05'
{
restore
preserve
keep if s3==1
g name    = "3sampleSD"
collapse (count) num_obs (sd) $varcare, by(name)
}
append using `05'
tempfile 06
save    `06'
{ //s5
restore
preserve
keep if s5==1
g name    = "5sample"
collapse (count) num_obs (mean) $varcare, by(name)
}
append using `06'
tempfile 09
save    `09'
{
restore
preserve
keep if s5==1
g name    = "5sampleSD"
collapse (count) num_obs (sd) $varcare, by(name)
}
append using `09'
tempfile 10
save    `10'
format $varcare %7.2f
format wg_c*    %7.3f
format age      %7.1f
format ned age tenure monthly  firmsize meanWage n_tenure* %7.0f
order name num_ob $varcare
sort  name 
outsheet using table_`tn', replace
}
{ //T2
local tn = "2"
use    `A_0', clear
cap drop y
g y = ay1
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem') replace 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem')  append							
}
cap drop y
g y = ay2
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = ay3
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							
cap drop y
g y = ay4
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = ay5
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = ay6
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							
!rm -f             table_`tn'.txt
}
{ //T3
local tm = "3"
use if es using "Data.dta", clear
g         meanWage_a   =               meanWage
replace   meanWage_a   = (  firmsize*  meanWage    -monthly_wage_0 )/(  firmsize-1) if   firmsize>1 & ///
(  ///
end_m==2 & end_d>=11 | ///
end_m==3              | /// 
end_m==5 & end_d>=11 | ///
end_m==6             | ///
end_m==8 & end_d>=11 | ///
end_m==9             | /// 
end_m==11 & end_d>=11 | ///
end_m==12            )
g       n_meanWage_a   =             n_meanWage 
replace n_meanWage_a   = (n_firmsize*n_meanWage    -monthly_wage_n0)/(n_firmsize-1) if n_firmsize>1  & ///
(n_start_m==1 			| ///
n_start_m==2 & n_start_d<11     | ///
n_start_m==4  		        | ///
n_start_m==5 & n_start_d<11     | ///
n_start_m==7                    | ///
n_start_m==8 & n_start_d<11     | ///
n_start_m==10                   | ///
n_start_m==11 & n_start_d<11)

keep if firmsize>=10  & firmsize!=.
drop if ay4==.
tempfile A_100
save    `A_100'

foreach ed in 3 1{  
local tn = "`tm'_`ed'"
use    `A_100', clear
drop if exp_r== `ed'
g       y =  n_firmsize_4 
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem') replace 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem')  append							
}

cap drop y
local nen = 1
g       y = n_firmsize_4 > firmsize_4
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem')  append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem')  append							
}
cap drop y
g y = n_mfirmsize/n_firmsize - mfirmsize/firmsize
	qui sum y                    , d
	qui replace y = r(p99) if y>r(p99) & y!=.
	qui replace y = r(p1)  if y<r(p1)
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = (n_workerage-workerage)/workerage
	qui sum y                    , d
	qui replace y = r(p99) if y>r(p99) & y!=.
	qui replace y = r(p1)  if y<r(p1)
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = ln(n_meanWage_a)-ln(meanWage_a)
	qui sum y                    , d
	qui replace y = r(p99) if y>r(p99) & y!=.
	qui replace y = r(p1)  if y<r(p1)
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							

cap drop y
g y = ay4 
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							
cap drop y
g y =  ln(n_meanWage)-ln(meanWage) 
	qui sum y                    , d
	qui replace y = r(p99) if y>r(p99) & y!=.
	qui replace y = r(p1)  if y<r(p1)
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = (n_mean_adjwage_2 )-(mean_adjwage_2 )
	qui sum y                    , d
	replace y = r(p99) if y>r(p99) & y!=.
	replace y = r(p1)  if y<r(p1)
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
!rm -f             table_`tn'.txt
}


}
{ //T4
local tn = "4"
use if es using "Data.dta", clear
g       y = n_tenure     if n_start-end<2*365 
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem') replace 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem')  append							
}

cap drop y
local nen = 1
g       y = n_tenure<`nen'*365 if n_start-end<2*365 & n_tenure!=. 
replace y = 0                  if n_start-end<2*365 & (19083-n_start>`nen'*365 & sample==11 & n_tenure==.)
replace y = 0                  if n_start-end<2*365 & (18293-n_start>`nen'*365 & sample!=11 & n_tenure==.)
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem')  append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem')  append							
}
cap drop y
g z = lwagen1 - lwagen0    if n_start-end<2*365
format z %9.0f
winsor z, g(y) p(.1)
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y z
g y = lwagen1 > lwagen0 if !mi(lwagen1) & n_start-end<2*365
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = n_benr!=benr         if !mi(benr) & !mi(n_benr) & n_start-end<2*365
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							

cap drop y
g y = n_industry!=industry if !mi(industry) & !mi(n_industry) & n_start-end<2*365 
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							
cap drop y
g y = n_blue!=blue  if !mi(blue) & !mi(n_blue) & n_start-end<2*365
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
rename n_region     n_regionold
recode n_regionold (8=10)(0=1)(.=99)(13=99)(60=99)(-105/-1=99),gen(n_region) 
g y = n_region!=region     if !mi(regionold) & !mi(n_region) & n_start-end<2*365
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = n_district!=district     if !mi(district) & !mi(n_district) & n_start-end<2*365
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = n_zipcode!=zipcode     if !mi(zipcode) & !mi(n_zipcode) & n_start-end<2*365
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = n_county!=county     if !mi(county) & !mi(n_county) & n_start-end<2*365
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = n_full  if !mi(full) & !mi(n_full)
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}

cap drop y
g y = n_nace95!=nace95 if !mi(nace95) & !mi(n_nace95) & n_start-end<2*365 
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							

!rm -f             table_`tn'.txt
}
}
{ //Remaining Tables of the Appendix (A1 and A9 were produced before)
{ //Table A2
local tn = "A02"
use    `A_0', clear
cap drop y
g tre = 0
gl vv2 ay1 ay2 ay3 ay4 ay5 ay6 
foreach   y1  of varlist $vv2 {
qui sum  `y1' if abs(age-40)<3
local     mem = r(mean)
local r1 = 2
local r3 = 10							
	qui reg `y1'  age40 $varp2
		   if tre==0 { 
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') replace							
			}
			else {
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
			}
local r1 = 3
local r3 = 10			
	qui reg `y1'  age40 $varp3
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 4
local r3 = 10
	qui reg `y1'  age40 $varp4
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 2
local r2 = 5
	qui reg `y1'  age40 $varp2 if abs(age-40)<`r2'
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append
local r1 = 1
local r2 = 2
	qui reg `y1'  age40 $varp1 if abs(age-40)<`r2'
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append							
	qui altrdrobust `y1'  age, c(40) all bwselect(CCT)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
	qui altrdrobust `y1'  age, c(40) all bwselect(IK)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("Mean", `mem', "PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
/*	altrdrobust `y1'  age, c(40) all bwselect(CV)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append			
*/
replace tre = 1
}
!rm -f             table_`tn'.txt
}
{ //Table A3
local tn = "A03" 
use if es using "Data.dta", clear
cap drop y
g       tre = 0
g       ty1 = tenure>1*365 if tenure!=.
g       ty2 = tenure>2*365 if tenure!=.
g       ty5 = tenure>5*365 if tenure!=. 
gl vv2 female blue marr austr lwage0 exp_r tenure ty1 ty5 
foreach   y1  of varlist $vv2 {
qui sum  `y1' if abs(age-40)<3
local     mem = r(mean)
local r1 = 2
local r3 = 10							
	qui reg `y1'  age40 $varp2
		   if tre==0 { 
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') replace							
			}
			else {
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
			}
local r1 = 3
local r3 = 10			
	qui reg `y1'  age40 $varp3
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 4
local r3 = 10
	qui reg `y1'  age40 $varp4
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 2
local r2 = 5
	qui reg `y1'  age40 $varp2 if abs(age-40)<`r2'
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append
local r1 = 1
local r2 = 2
	qui reg `y1'  age40 $varp1 if abs(age-40)<`r2'
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append							
	qui altrdrobust `y1'  age, c(40) all bwselect(CCT)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
	qui altrdrobust `y1'  age, c(40) all bwselect(IK)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("Mean", `mem', "PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
/*	altrdrobust `y1'  age, c(40) all bwselect(CV)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append			
*/
replace tre = 1
}
!rm -f             table_`tn'.txt
}


{ //Table A4
local tn = "A04"
use    `A_0', clear
gl vv2 ay1 ay2 ay3 ay4 ay5 ay6
cap drop tre
g tre = 0
foreach   y1  of varlist $vv2 {
qui sum  p`y1' if abs(age-40)<3
local     mem = r(mean)
local r1 = 2
local r3 = 10							
	qui reg p`y1'  age40 $varp2 if !mi(`y1')
		   if tre==0 { 
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') replace							
			}
			else {
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
			}
local r1 = 3
local r3 = 10			
	qui reg p`y1'  age40 $varp3 if !mi(`y1')
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 4
local r3 = 10
	qui reg p`y1'  age40 $varp4 if !mi(`y1')
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 2
local r2 = 5
	qui reg p`y1'  age40 $varp2 if abs(age-40)<`r2' & !mi(`y1')
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append
local r1 = 1
local r2 = 2
	qui reg p`y1'  age40 $varp1 if abs(age-40)<`r2' & !mi(`y1')
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append							
	qui altrdrobust p`y1'  age if !mi(`y1'), c(40) all bwselect(CCT) //if !mi(`yy')
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
	qui altrdrobust p`y1'  age if !mi(`y1'), c(40) all bwselect(IK) 
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("Mean", `mem', "PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
replace tre = 1
}
!rm -f             table_`tn'.txt
}

{ //Table A5
local tm = "A05" 
use  "Data_I.dta", clear
keep if clay==1
keep if age>30 & age<50
gl vv2 ay1 ay2 ay3 ay4 ay5 ay6 
{ 
g age_p1 = age^1
g age_p2 = age^2
g age_p3 = age^3
g age_p4 = age^4
g age40 = age>=40
g age_q1 = ((age-40)^1)*age40
g age_q2 = ((age-40)^2)*age40
g age_q3 = ((age-40)^3)*age40
g age_q4 = ((age-40)^4)*age40
g ay1 = ned if n_start-end<2*365
g ay2 = ned<=30*7
g ay3 = ned<=39*7
g ay4 = wg_c if n_start-end<2*365
g ay5 = lwagen0 if n_start-end<2*365 & !(recall==1 & end_y==year(n_start))
g ay6 = lwagen0>lwage0+ln(.5) if n_start-end<2*365 & lwagen0 !=. & lwage0 !=. & !(recall==1 & end_y==year(n_start))

g        c = 0 if end >= mdy(8,1,1989) &   work5>3*365 & work10>6*365 
replace  c = 1 if end >= mdy(8,1,1989) &                 work10<6*365 
replace  c = 2 if end <  mdy(8,1,1988)                                
drop if  mi(c)

gl fvar c
	egen  tenure_c        = xtile(tenure), by($fvar)  n(10)
foreach ii in 2 5{	
	egen  work`ii'_c = xtile(work`ii' ), by($fvar) n(10)
}
replace recall_rate   = 0 if recall_rate  ==.  
replace recallrate_4  = 0 if recallrate_4 ==.

g       dexp_r = exp_r  
replace dexp_r = 2 if exp_r==.
 
local bin = 10 
	g       malefrac      = mfirmsize/firmsize
	replace malefrac      = 0 if firmsize==0
	egen    firmsize_c      = xtile(firmsize     ), by($fvar) n(`bin')
	egen    malefrac_c      = xtile(malefrac     ), by($fvar) n(`bin')	
	egen   workerage_c      = xtile(workerage    ), by($fvar) n(`bin')
	egen    meanWage_c      = xtile(meanWage     ), by($fvar) n(`bin')
	egen  recallrate_c      = xtile(recall_rate  ), by($fvar) n(`bin')
	egen  recallrate_4c     = xtile(recallrate_4 ), by($fvar) n(`bin')
	g       recalls_bl3yc   =      recalls_benr_last3years 
	replace recalls_bl3yc   = 7 if recalls_benr_last3years>7 
	g       rrecalls_l3yc   =      recalls_last3years 
	replace rrecalls_l3yc   = 7 if recalls_last3years>7
egen  wage0_pc  = xtile(lwage0) , n(100) by(end_y c)
foreach yy in exp_r zipcode firmsize_c malefrac_c workerage_c meanWage_c  wage0_pc {	
	qui sum     `yy', d 
	qui g       `yy'_m = `yy'
	qui replace `yy'_m = r(max) + 1 if `yy' ==.
}

local trer = 0
foreach   y1  of varlist $vv2 {
local `trer' = `tre'+1
if `trer' !=4 {
 qui reg `y1' $cova 
}
if `trer' ==4 {
 qui reg `y1' $cova i.wage0_pc
}
predict p`y1',res
replace `y1' = p`y1'
cap drop p`y1'
}
}
tempfile A_10
save    `A_10'
foreach cic in 0 1 2{
use     `A_10', clear
keep if c==`cic'
local tn = "`tm'_`cic'"
disp "`tn'"          
cap drop y
cap drop tre
g tre = 0
foreach   y1  of varlist $vv2 {
qui sum  `y1' if abs(age-40)<3
local     mem = r(mean)
local r1 = 2
local r3 = 10							
	qui reg `y1'  age40 $varp2
		   if tre==0 { 
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') replace							
			}
			else {
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
			}
local r1 = 3
local r3 = 10			
	qui reg `y1'  age40 $varp3
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 4
local r3 = 10
	qui reg `y1'  age40 $varp4
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 2
local r2 = 5
	qui reg `y1'  age40 $varp2 if abs(age-40)<`r2'
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append
local r1 = 1
local r2 = 2
	qui reg `y1'  age40 $varp1 if abs(age-40)<`r2'
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append							
	qui altrdrobust `y1'  age, c(40) all bwselect(CCT)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
	qui altrdrobust `y1'  age, c(40) all bwselect(IK)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("Mean", `mem', "PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
replace tre = 1
}
!rm -f             table_`tn'.txt
}
}
{ //Table A6
local tn = "A06"
use    `A_0', clear
cap drop y
g tre = 0
gl vv2 ay1 ay2 ay3 ay4 ay5 ay6 
foreach   y1  of varlist $vv2 {
qui sum  `y1' if abs(age-40)<3
local     mem = r(mean)
qui reg `y1' i.end_m#i.birthmo
qui predict a`y1', res
local r1 = 2
local r3 = 10							
	qui reg a`y1'  age40 $varp2
		   if tre==0 { 
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') replace							
			}
			else {
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
			}
local r1 = 3
local r3 = 10			
	qui reg a`y1'  age40 $varp3
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 4
local r3 = 10
	qui reg a`y1'  age40 $varp4
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r3') append
local r1 = 2
local r2 = 5
	qui reg a`y1'  age40 $varp2 if abs(age-40)<`r2'
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append
local r1 = 1
local r2 = 2
	qui reg a`y1'  age40 $varp1 if abs(age-40)<`r2'
			qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("PD", `r1', "B1", `r2') append							
	qui altrdrobust a`y1'  age, c(40) all bwselect(CCT)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
	qui altrdrobust a`y1'  age, c(40) all bwselect(IK)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("Mean", `mem', "PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append
/*	altrdrobust `y1'  age, c(40) all bwselect(CV)
local r0  = e(N)
local r1  = e(p)
local r1b = e(q)
local r2  = e(h_bw)
local r2b = e(b_bw)
			qui outreg2 using table_`tn',  noparen nocons excel             ///
			addstat("PD", `r1', "B1", `r2', "PDb", `r1b', "B1b", `r2b')  append			
*/
replace tre = 1
}
!rm -f             table_`tn'.txt
}

{ //Table A7
local tn = "A07"
use    `A_0', clear
keep if mpay2==1
{
cap drop y
g y = ay1
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') replace 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay2
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay3
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay4
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay5
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay6
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}	
}
use    `A_0', clear
keep if mpay2==2
{
cap drop y
g y = ay1
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay2
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay3
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay4
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay5
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}
cap drop y
g y = ay6
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem') append							
}	
}
!rm -f             table_`tn'.txt
}
{ //Table A11
local tn = "A11"
use    `A_0', clear
	local ii = 1
	replace age = (int(age*`ii')/`ii')+1/(2*`ii')
drop age_p* age_q* age40
g age_p1 = age^1
g age_p2 = age^2
g age40  = age>=40
g age_q1 = ((age-40)^1)*age40
g age_q2 = ((age-40)^2)*age40
cap drop y
g y = ay1
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem') replace 
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40) addstat("Mean", `mem')  append							
}
cap drop y
g y = ay2
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = ay3
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							
cap drop y
g y = ay4
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = ay5
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova i.wage0_pc
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}
cap drop y
g y = ay6
{
qui sum y if abs(age-40)<3
local mem = r(mean)
qui reg y  age40 $varp2
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
qui reg y  age40 $varp2 $cova
											qui outreg2 using table_`tn',  noparen nocons excel keep(age40)  addstat("Mean", `mem')  append
}							
!rm -f             table_`tn'.txt
}

}
