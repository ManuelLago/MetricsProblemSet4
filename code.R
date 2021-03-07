library(tidyverse)
library(haven)
library(dplyr)
library(stargazer)
library(rdrobust)

dataemp <- read_dta(file = "C:\\Users\\btpta\\Desktop\\Metrics 2\\PS 4\\dataset_ps4.dta")
attach(dataemp)

#generating bins
i <- 3
dataemp <- dataemp  %>% add_column(agegroup = (ceiling(age*i)/i)+1/(2*i)) %>%
  group_by(agegroup) %>%
  #mutate(id = cur_group_id())%>%
  mutate(gn = n()) %>%
  mutate(dens = gn/nrow(dataemp))

#Figure 1 Density of age
rdplot( dataemp$dens, dataemp$age, c=40, p=2, masspoints="off", title="Density of Age at Layoff", x.label = "Age", y.label ="Density")

#Figure 2 Previous wage at the threshold
dataemp <- dataemp %>% 
  group_by(agegroup) %>%
  mutate(avwage0 = mean(as.numeric(lwage0),na.rm=T))
rdplot( dataemp$avwage0, dataemp$agegroup, c=40, p=2, masspoints="off", title="Previous wage at the threshold", 
        x.label = "Age", y.label ="Log Previous Wage")

#Figure 3 Nonemployment Duration
dataemp <- dataemp %>%
  group_by(agegroup) %>%
  mutate(avnonem = mean(as.numeric(nonemp),na.rm=T))
rdplot( dataemp$avnonem, dataemp$agegroup, c=40, p=2, masspoints="off", title="Unemployment Benefit Extension and Nonemployment Duration", 
        x.label = "Age", y.label ="Non-Emplyment Duration")

#Figure 4 Probability of Finding a Job after 39 Weeks

dataemp <- dataemp %>% 
  group_by(agegroup) %>%
  mutate(avfindjob = mean(as.numeric(jobfind),na.rm=T))
rdplot(dataemp$avfindjob, dataemp$agegroup, c=40, p=2,  masspoints="off", title="Finding a Job within 39 Weeks", 
       x.label = "Age", y.label ="Probability of finding job")

#Figure 5 New wage around threshhold
dataemp <- dataemp %>% 
  group_by(agegroup) %>%
  mutate(avwage1 = mean(as.numeric(lwage1),na.rm=T))
rdplot( dataemp$avwage1, dataemp$agegroup, c=40, p=2, masspoints="off", title="New wage at the threshold", 
        x.label = "Age", y.label ="Log Previous Wage")


rdplot( dataemp$avwage1, dataemp$agegroup, c=40, p=2, masspoints="off",  title="New wage at the threshold", 
        x.label = "Age", y.label ="Log Previous Wage")

##########Ex 5
cutoff=40
dataemp$age40=0
dataemp$age40[age>cutoff]=1
attach(dataemp)
#centered regressors
dataemp$age_p1=(age-cutoff)^1
dataemp$age_p2=(age-cutoff)^2
dataemp$age_p3=(age-cutoff)^3
dataemp$age_p4=(age-cutoff)^4

dataemp$age_q1=(age-cutoff)^1*age40
dataemp$age_q2=(age-cutoff)^2*age40
dataemp$age_q3=(age-cutoff)^3*age40
dataemp$age_q4=(age-cutoff)^4*age40

#Table 1
#Panel 1 - Second order polynomal 
model1<-lm(nonemp ~ age40+age_p1+age_p2+age_q1+age_q2, data=dataemp)  #noneplyment duration
model2<-lm(jobfind~age40+age_p1+age_p2+age_q1+age_q2, data=dataemp) #Prob of finding a job within 39
model3<-lm(lwage1~ age40+age_p1+age_p2+age_q1+age_q2, data=dataemp) #re-employment wage

star.out.1 <- stargazer(model1, model2, model3,  align=F, dep.var.labels=c("Non-Employment", "New Job 39 Weeks", "Post-Wages"),
                        covariate.labels= c("Treatment"), omit=c("age_p1", "age_p2", "age_q1", "age_q2", "Constant"), keep.stat="n", 
                        add.lines=list(c("Bandwith/Full Sample", "Full Sample", "Full Sample", "Full Sample"), c("Polynomial Degree", "2",  "2", "2")), column.sep.width = "1pt")
#Panel 2 - Second order polynomal with bandwith +/- 5 years
bandwith=5
datalim<-subset(dataemp, abs(age_p1)<=bandwith)

model4<-lm(nonemp ~ age40+age_p1+age_p2+age_q1+age_q2, data=datalim)  #noneplyment duration
model5<-lm(jobfind~age40+age_p1+age_p2+age_q1+age_q2, data=datalim) #Prob of finding a job within 39
model6<-lm(lwage1~ age40+age_p1+age_p2+age_q1+age_q2, data=datalim) #re-employment wage

star.out.2 <- stargazer(model4, model5, model6,  align=T, dep.var.labels=c("Non-Employment Duration", "New Job Within 39 Weeks", "Re-Employment Wages"),
                        covariate.labels= c("Treatment"), omit=c("age_p1", "age_p2", "age_q1", "age_q2","Constant"), keep.stat="n", 
                        add.lines=list(c("Bandwith/Full Sample", "5", "5", "5"), c("Polynomial Degree", "2",  "2", "2"))
)

#Panel 3 - Linear Controls
model7<-lm(nonemp ~ age40+age_p1+age_q1, data=dataemp)  #noneplyment duration
model8<-lm(jobfind~age40+age_p1+age_q1, data=dataemp) #Prob of finding a job within 39
model9<-lm(lwage1~ age40+age_p1+age_q1, data=dataemp) #re-employment wage

star.out.3 <- stargazer(model7, model8, model9,  align=T, dep.var.labels=c("Non-Employment Duration", "New Job Within 39 Weeks", "Re-Employment Wages"),
                        covariate.labels= c("Treatment"), omit=c("age_p1", "age_q1", "Constant"), keep.stat="n", 
                        add.lines=list(c("Bandwith/Full Sample", "Full Sample", "Full Sample", "Full Sample"), c("Polynomial Degree", "1",  "1", "1")))

#Panel 4 - Fourth order polynomial

model10<-lm(nonemp ~ age40+age_p1+age_p2+age_p3+age_p4+age_q1+age_q2+age_q3+age_q4, data=dataemp)  #noneplyment duration
model11<-lm(jobfind ~ age40+age_p1+age_p2+age_p3+age_p4+age_q1+age_q2+age_q3+age_q4, data=dataemp) #Prob of finding a job within 39
model12<-lm(lwage1 ~ age40+age_p1+age_p2+age_p3+age_p4+age_q1+age_q2+age_q3+age_q4, data=dataemp) #re-employment wage


star.out.4<- stargazer(model10, model11, model12,  align=T, dep.var.labels=c("Non-Employment Duration", "New Job Within 39 Weeks", "Re-Employment Wages"),
                       covariate.labels= c("Treatment"), omit=c("age_p1", "age_p2","age_p3", "age_p4", "age_q1", "age_q2","age_q3", "age_q4", "Constant"), keep.stat="n", 
                       add.lines=list(c("Bandwith/Full Sample", "Full Sample", "Full Sample", "Full Sample"), c("Polynomial Degree", "4",  "4", "4")))

#Panel 5 - Nonparametric estimation

model13<- rdrobust(dataemp$nonemp, dataemp$age, c=40, all=T, masspoints="off")   #noneplyment duration
model14<- rdrobust(dataemp$jobfind, dataemp$age, c=40, all=T, masspoints="off")  #Prob of finding a job within 39
model15<- rdrobust(dataemp$lwage1, dataemp$age, c=40, all=T, masspoints="off")  #re-employment wage

star.out.5<- stargazer(model13, model14, model15,  align=T, dep.var.labels=c("Non-Employment Duration", 
                                                                             "New Job Within 39 Weeks", 
                                                                             "Re-Employment Wages"),
                       covariate.labels= c("Treatment"), keep.stat="n", 
                       add.lines=list(c("Estimated Bandwith", "2.833", "3.437", "3.382"), 
                                      c("Polynomial Degree", "1",  "1", "1")))


Tablestar <- star_panel (star.out.1, star.out.2, star.out.3, star.out.4, same.summary.stats = FALSE,
                         panel.names = c("Second order polynomals", "Second order polynomals with Bandwith",
                                         "Linear Controls", "Fourth order polynomial", "Optimal Bandwidth with rdrobust" ))
star_tex_write(Tablestar, file = "my_tex_file2.tex", headers = TRUE)
