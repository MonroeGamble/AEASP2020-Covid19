* J Monroe Gamble IV
* Created 07/03/2020
* Covid-19 Project\data
* Outputs regression tables using R output. Run R code then this file.

clear

*Set Directory
cd "***CONFIDENTIAL***/data"

*Import data
import delimited using 0_memorial_Day_TODAY2020-07-23.csv

*Save & Open Stata file
*use 0_memorial_Day
*save 0_memorial_Day

*Data Manipulation
*gen other = county_pop - (nhba_tot + nhwa_tot) 
*replace percother = (other/county_pop)*100
gen term_end_soon=0
replace term_end_soon=1 if term_end<=2021 /*Interaction w/ Party */
drop if stname == "Alaska"
drop if stname == "Hawaii"

drop if date == "2020-06-07"

gen at_risk = age65to69years + age70to74years + age75to79years + age80to84years + age85yearsorolder
gen perc_at_risk = (at_risk/county_pop)*100

rename pop_density_per_mi pop_density_mi
rename republican rep_governor

*Set Directory
cd "G:\Shared drives\Monroe\Lenovo\AEASP\ECSP 491 Research Methods\Project\STATA"


****** Regressions ******************
	
	global pop_par county_pop pop_density_mi perc_at_risk

	global politics hillaryperc trumpperc rep_governor term_end_soon 
	
	global economic povpercent medincome percapita
	
	global racePer percblack percwhite perchisp 
	

	
		* Table 1. After 2020-06-07
			
			
					*Cases
					reg cases_per $pop_par $racePer
					estimates table, star(.05 .01 .001)
					outreg2 using Case_14POST.doc, replace ///
					title(Table 1. Cases After Memorial Day) se symbol(***,**,*) ///
					ctitle(Demographic) dec(6)
					
					reg cases_per $pop_par $racePer $politics
					estimates table, star(.05 .01 .001)
					outreg2 using Case_14POST.doc, append se symbol(***,**,*) ///
					ctitle(Political) dec(6)
					
					regress cases_per $pop_par $racePer $politics $economic i.region
					estimates table, star(.05 .01 .001)
					outreg2 using Case_14POST.doc, append se symbol(***,**,*) ///
					ctitle(Political & Economic) 
					
					
					*Deaths
					reg deaths_per $pop_par $racePer
					estimates table, star(.05 .01 .001)
					outreg2 using Death_14POST.doc, replace ///
					title(Table 2. Deaths After Memorial Day) se symbol(***,**,*) ///
					ctitle(Demographic) dec(6)
					
					reg deaths_per $pop_par $racePer $politics
					estimates table, star(.05 .01 .001)
					outreg2 using Death_14POST.doc, append se symbol(***,**,*) ///
					ctitle(Political) dec(6)
					
					regress deaths_per $pop_par $racePer $politics $economic i.region
					estimates table, star(.05 .01 .001)
					outreg2 using Death_14POST.doc, append se symbol(***,**,*) ///
					ctitle(Political & Economic) dec(6)

					*******************************************************
	
	*Deaths by Region Bar Chart
	tabstat deaths, by(state) stat(max)
	graph bar (max) deaths, over(region) title("Deaths By Region After")
	
	*Total Cases by Region Bar Chart
	tabstat cases, by(state) stat(max)
	graph bar (max) cases, over(region) title("Cases By Region After")
		
		

		**************************************************************************

cd "G:\Shared drives\Monroe\Lenovo\AEASP\ECSP 491 Research Methods\Project\data"

clear

*Import data
import delimited using 0_memorial_Day


*Data Manipulation
*gen other = county_pop - (nhba_tot + nhwa_tot) /*drop nhwa*/
*replace percother = (other/county_pop)*100
gen term_end_soon=0
replace term_end_soon=1 if term_end<=2021 /*Interaction w/ Party */
drop if stname == "Alaska"
drop if stname == "Hawaii"

drop if date == "2020-07-15"

gen at_risk = age65to69years + age70to74years + age75to79years + age80to84years + age85yearsorolder
gen perc_at_risk = (at_risk/county_pop)*100

rename pop_density_per_mi pop_density_mi
rename republican rep_governor


*Set Directory
cd "G:\Shared drives\Monroe\Lenovo\AEASP\ECSP 491 Research Methods\Project\STATA"

		* Table 2. Before 2020-06-07
				
					*Cases
					reg cases_per $pop_par $racePer
					estimates table, star(.05 .01 .001)
					outreg2 using Case_14PRE.doc, replace ///
					title(Table 1. Cases Before Memorial Day) se symbol(***,**,*) ///
					ctitle(Demographic) dec(6)
					
					reg cases_per $pop_par $racePer $politics
					estimates table, star(.05 .01 .001)
					outreg2 using Case_14PRE.doc, append se symbol(***,**,*) ///
					ctitle(Political) dec(6)
					
					regress cases_per $pop_par $racePer $politics $economic i.region
					estimates table, star(.05 .01 .001)
					outreg2 using Case_14PRE.doc, append se symbol(***,**,*) ///
					ctitle(Political & Economic) 
					
					
					*Deaths
					reg deaths_per $pop_par $racePer
					estimates table, star(.05 .01 .001)
					outreg2 using Death_14PRE.doc, replace ///
					title(Table 2. Deaths Before Memorial Day) se symbol(***,**,*) ///
					ctitle(Demographic) dec(6)
					
					reg deaths_per $pop_par $racePer $politics
					estimates table, star(.05 .01 .001)
					outreg2 using Death_14PRE.doc, append se symbol(***,**,*) ///
					ctitle(Political) dec(6)
					
					regress deaths_per $pop_par $racePer $politics $economic i.region
					estimates table, star(.05 .01 .001)
					outreg2 using Death_14PRE.doc, append se symbol(***,**,*) ///
					ctitle(Political & Economic) dec(6)
					
*******************************************************
	
	
	*Deaths by Region Bar Chart
	tabstat deaths, by(state) stat(max)
	graph bar (max) deaths, over(region) title("Deaths By Region Before")
	
	*Total Cases by Region Bar Chart
	tabstat cases, by(state) stat(max)
	graph bar (max) cases, over(region) title("Cases By Region Before")
		
					
	*1 Northeast Region
	*2 Midwest Region
	*3 South Region
	*4 West Region

