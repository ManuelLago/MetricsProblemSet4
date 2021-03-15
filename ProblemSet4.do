local ii = 3
 gen   agecluster = (int(age*`ii')/`ii')+1/(2*`ii')
 bysort agecluster : gen id = _n
 gen total = _N
 bys agecluster : gen nec = _N
 replace nec = nec / total
 
 *question 2 Density//
twoway ///
(scatter nec   agecluster   if id==1, mcolor(black) msize(small)) ///
(qfit    nec   agecluster   if id==1 & age>40, lcolor(black) ) ///
(qfit    nec   agecluster   if id==1 & age<40, lcolor(black) ) ///
, xline(40, lp(dash) lcolor(gs10)) ///
xt("Age at layoff") yt("Density") leg(off) graphregion(fcolor(white)) ///
subtitle("Panel a: Distribution of Age",size(small)) name(F1A, replace)

*question 2 logwage
bysort agecluster : egen av_prevwage = mean( lwage0 )
twoway ///
(scatter av_prevwage   agec if   id==1, mcolor(black) msize(small)) /// 
(qfit    lwage0    age   if   age>=40, lcolor(black)) ///
(qfit    lwage0    age   if   age<40 , lcolor(black)) ///
, xline(40, lp(dash) lcolor(gs10)) graphregion(fcolor(white)) legend(off)  ///
ytitle("Log previous wage") xt("Age at layoff") 

*question 4 non-employment duration
bysort agecluster : egen av_nonemp = mean(nonemp)
twoway ///
(scatter av_nonemp    agecluster if   id==1, mcolor(black) msize(small)) /// 
(qfit    nonemp    age   if   age>=40, lcolor(black) ) ///
(qfit    nonemp    age   if   age<40 , lcolor(black) ) ///
, xline(40, lp(dash) lcolor(gs10)) graphregion(fcolor(white)) legend(off)  ytitle("`tit'") xtitle("Age at layoff") subtitle("Panel a ",size(small) ) name(F3a, replace)



*question4 probability of finding a job
bysort agecluster : egen avjobfind = mean(jobfind)
twoway ///
(scatter avjobfind    agecluster if   id==1, mcolor(black) msize(small)) /// 
(qfit    jobfind    age   if   age>=40, lcolor(black) ) ///
(qfit    jobfind   age   if   age<40 , lcolor(black) ) ///
, xline(40, lp(dash) lcolor(gs10)) graphregion(fcolor(white)) legend(off) ///
  ytitle("`Probability of finding job'")   xtitle("Age at layoff") subtitle("Panel b ",size(small))
  
*question4 logwage
bysort agecluster : egen avernewwagenewwage = mean( lwage1 )
twoway ///
(scatter avernewwagenewwage    agecluster if   id==1, mcolor(black) msize(small)) /// 
(qfit    lwage1    age   if   age>=40, lcolor(black) ) ///
(qfit    lwage1    age   if   age<40 , lcolor(black) ) ///
, xline(40, lp(dash) lcolor(gs10)) graphregion(fcolor(white)) legend(off)  ytitle("`tit'") xtitle("Age at layoff") subtitle("Panel c",size(small) ) name(F3c, replace)

