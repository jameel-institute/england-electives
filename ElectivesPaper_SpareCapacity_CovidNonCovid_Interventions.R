###Adapting hospital capacity to meet changing demands during the COVID-19 pandemic 
###This can be viewed at: https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-06-15-COVID19-Report-27.pdf

###code to calculate spare capacity under varying numbers of elective and COVID-19 patients and different intervention scenarios

##Input baseline capacity, covid related variables and intervention parameters
##This corresponds to Table 2 and Supplementary Table 2 in Report 27

#baseline capacity
baseline_data <- list(total_cc_beds=4114,
                      total_ga_beds=99569,
                      cc_occupied_non_covid=3297,
                      ga_occupied_non_covid=89800,
                      prop_non_covid_vent=0.43,
                      total_vents=8175,
                      cc_nurses_fte=3939,
                      ga_nurses_fte=32354,
                      cc_sen_doc_fte=965,
                      cc_jun_doc_fte=677,
                      ga_sen_doc_fte=12680,
                      ga_jun_doc_fte=10293,
                      cc_bed_nurse=1,
                      ga_bed_nurse=5,
                      cc_bed_sen_doc=15,
                      cc_bed_jun_doc=8,
                      ga_bed_sen_doc=15,
                      ga_bed_jun_doc=15,
                      headcount_to_fte=0.88,
                      pop_size=55977200)

#covid related variables
covid_data <- list(covid_cc_num=3100,
                   covid_ga_num=15700,
                   prop_covid_vent=0.63,
                   nurse_covid_sickness=0.92,
                   doc_covid_sickness=0.86)

#intervention parametrisation
intervention_data <- list(field_cc=500,
                          field_ga=8000,
                          private_beds=8000,
                          private_ventilators=1200,
                          private_nurse_headcount=10000,
                          private_doctor_headcount=700,
                          return_nurse_gain=6147,
                          return_doctor_gain=2660,
                          student_nurse=18700,
                          student_doc=5500)


##this function calculates spare capacity under a range of elective patient numbers for different intervention scenarios for a fixed number of covid patients
hospital_capacity_electives_surgery <- function(baseline_vector, covid_vector, intervention_vector, 
                                                ga_non_covid_surgery=seq(52982,89800,500)){
  
  spare_capacity <- NULL  
  ##iterate over different numbers of elective patients - 52982 is the number of non-elective G&A patients occupying beds (HES data)
  for(k in 1:length(ga_non_covid_surgery)){                                                                  

    #use the derived relationship between G&A and CC elective patients (report appendix)
    non_covid_ga <- round((ga_non_covid_surgery[k]))
    non_covid_cc <- round(0.03*non_covid_ga + 603)
    
    ##get the proportion of G&A and CC resources at baseline (beds & staff) for future use in intervention scenarios to allocate resources among different categories
    cc_ga_bed_split <- baseline_vector$total_cc_beds/(baseline_vector$total_cc_beds+baseline_vector$total_ga_beds)
    cc_ga_nurse_split <- baseline_vector$cc_nurses_fte/(baseline_vector$cc_nurses_fte+baseline_vector$ga_nurses_fte)
    cc_jun_doc_prop <- baseline_vector$cc_jun_doc_fte/(baseline_vector$cc_jun_doc_fte+baseline_vector$ga_jun_doc_fte
                                                       +baseline_vector$cc_sen_doc_fte+baseline_vector$ga_sen_doc_fte)
    ga_jun_doc_prop <- baseline_vector$ga_jun_doc_fte/(baseline_vector$cc_jun_doc_fte+baseline_vector$ga_jun_doc_fte
                                             +baseline_vector$cc_sen_doc_fte+baseline_vector$ga_sen_doc_fte)
    cc_sen_doc_prop <- baseline_vector$cc_sen_doc_fte/(baseline_vector$cc_jun_doc_fte+baseline_vector$ga_jun_doc_fte
                                            +baseline_vector$cc_sen_doc_fte+baseline_vector$ga_sen_doc_fte)
    ga_sen_doc_prop <- baseline_vector$ga_sen_doc_fte/(baseline_vector$cc_jun_doc_fte+baseline_vector$ga_jun_doc_fte
                                             +baseline_vector$cc_sen_doc_fte+baseline_vector$ga_sen_doc_fte)
    
    ##Calculate the total number of patients requiring beds and ventilation (COVID-19 & non-COVID-19)
    cc_patients <- covid_vector$covid_cc_num + non_covid_cc
    ga_patients <- covid_vector$covid_ga_num + non_covid_ga
    ventilator_need <- round((non_covid_cc * baseline_vector$prop_non_covid_vent) + (covid_vector$covid_cc_num * covid_vector$prop_covid_vent))
    
    ###BASELINE###
    
    ##Calculate spare capacity for each variable at baseline 
    cc_beds <- (baseline_vector$total_cc_beds - cc_patients) 
    ga_beds <- (baseline_vector$total_ga_beds - ga_patients) 
    vents <- (baseline_vector$total_vents - ventilator_need) 
    cc_nurses <- ((baseline_vector$cc_nurses_fte * covid_vector$nurse_covid_sickness) - (cc_patients/baseline_vector$cc_bed_nurse))
    ga_nurses <- ((baseline_vector$ga_nurses_fte * covid_vector$nurse_covid_sickness)  - (ga_patients/baseline_vector$ga_bed_nurse))
    cc_sen_doc <- ((baseline_vector$cc_sen_doc_fte * covid_vector$doc_covid_sickness) - (cc_patients/baseline_vector$cc_bed_sen_doc)) 
    cc_jun_doc <- ((baseline_vector$cc_jun_doc_fte * covid_vector$doc_covid_sickness) - (cc_patients/baseline_vector$cc_bed_jun_doc)) 
    ga_sen_doc <- ((baseline_vector$ga_sen_doc_fte * covid_vector$doc_covid_sickness) - (ga_patients/baseline_vector$ga_bed_sen_doc)) 
    ga_jun_doc <- ((baseline_vector$ga_jun_doc_fte * covid_vector$doc_covid_sickness) - (ga_patients/baseline_vector$ga_bed_jun_doc)) 
    
    baseline <- c(cc_beds, ga_beds, vents, cc_nurses, ga_nurses, cc_sen_doc, cc_jun_doc, ga_sen_doc, ga_jun_doc)
    baseline_gains <- rep(0, length(baseline))
    
    ###HOSPITAL PROVISION INTERVENTIONS###
    
    
    ##FIELD HOSPITALS
    
    field_hosp <- baseline;  field_hosp_gains <- baseline_gains
    
    ##increase in bed numbers
    field_hosp_cc <- baseline_vector$total_cc_beds + intervention_vector$field_cc
    field_hosp_ga <- baseline_vector$total_ga_beds + intervention_vector$field_ga
    
    ##calculate spare capacity under this increased supply
    field_hosp[1] <- (field_hosp_cc - cc_patients) 
    field_hosp[2] <- (field_hosp_ga - ga_patients) 
    
    ##record the increase in beds for the future combination scenarios
    field_hosp_gains[1] <- intervention_vector$field_cc
    field_hosp_gains[2] <- intervention_vector$field_ga
    
    
    ##PRIVATE RESOURCES
    
    private_res <- baseline;    private_res_gains <- baseline_gains
    
    ##increase in bed, staff and ventilator numbers
    private_cc_beds <-  baseline_vector$total_cc_beds + (cc_ga_bed_split * intervention_vector$private_beds) 
    private_ga_beds <- baseline_vector$total_ga_beds + ((1 - cc_ga_bed_split) * intervention_vector$private_beds)
    private_cc_nurses <- baseline_vector$cc_nurses_fte + (cc_ga_nurse_split * baseline_vector$headcount_to_fte * intervention_vector$private_nurse_headcount)
    private_ga_nurses <- baseline_vector$ga_nurses_fte + ((1- cc_ga_nurse_split) * baseline_vector$headcount_to_fte * intervention_vector$private_nurse_headcount)
    private_cc_sen_doc <- baseline_vector$cc_sen_doc_fte + (cc_sen_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$private_doctor_headcount)
    private_cc_jun_doc <- baseline_vector$cc_jun_doc_fte + (cc_jun_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$private_doctor_headcount)
    private_ga_sen_doc <- baseline_vector$ga_sen_doc_fte + (ga_sen_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$private_doctor_headcount)
    private_ga_jun_doc <- baseline_vector$ga_jun_doc_fte + (ga_jun_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$private_doctor_headcount)
    private_vents <- baseline_vector$total_vents + intervention_vector$private_ventilators
    
    ##calculate spare capacity under this increased supply
    private_res[1] <- (private_cc_beds - cc_patients)
    private_res[2] <- (private_ga_beds - ga_patients)
    private_res[3] <- (private_vents - ventilator_need)
    private_res[4] <- ((private_cc_nurses * covid_vector$nurse_covid_sickness) - cc_patients/baseline_vector$cc_bed_nurse)
    private_res[5] <- ((private_ga_nurses * covid_vector$nurse_covid_sickness) - ga_patients/baseline_vector$ga_bed_nurse)
    private_res[6] <- ((private_cc_sen_doc * covid_vector$doc_covid_sickness) - cc_patients/baseline_vector$cc_bed_sen_doc)
    private_res[7] <- ((private_cc_jun_doc * covid_vector$doc_covid_sickness) - cc_patients/baseline_vector$cc_bed_jun_doc)
    private_res[8] <- ((private_ga_sen_doc * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_sen_doc)
    private_res[9] <- ((private_ga_jun_doc * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_jun_doc)
    
    ##record the increase in resources for the future combination scenarios
    private_res_gains[1] <- (cc_ga_bed_split * intervention_vector$private_beds) 
    private_res_gains[2] <- ((1 - cc_ga_bed_split) * intervention_vector$private_beds)
    private_res_gains[3] <- intervention_vector$private_ventilators
    private_res_gains[4] <- (cc_ga_nurse_split * baseline_vector$headcount_to_fte * intervention_vector$private_nurse_headcount)
    private_res_gains[5] <- ((1- cc_ga_nurse_split) * baseline_vector$headcount_to_fte * intervention_vector$private_nurse_headcount)
    private_res_gains[6] <- (cc_sen_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$private_doctor_headcount)
    private_res_gains[7] <- (cc_jun_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$private_doctor_headcount)
    private_res_gains[8] <- (ga_sen_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$private_doctor_headcount)
    private_res_gains[9] <- (ga_jun_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$private_doctor_headcount)
    
    
    ##RETURN OF FORMER STAFF
    
    return_staff <- baseline;   return_gains <- baseline_gains 
    
    ##increase in staff numbers
    return_cc_nurses <-  baseline_vector$cc_nurses_fte + (cc_ga_nurse_split * baseline_vector$headcount_to_fte * intervention_vector$return_nurse_gain)
    return_ga_nurses <-  baseline_vector$ga_nurses_fte + ((1- cc_ga_nurse_split) * baseline_vector$headcount_to_fte * intervention_vector$return_nurse_gain)
    return_cc_sen_doc <- baseline_vector$cc_sen_doc_fte + (cc_sen_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$return_doctor_gain)
    return_cc_jun_doc <- baseline_vector$cc_jun_doc_fte + (cc_jun_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$return_doctor_gain)
    return_ga_sen_doc <- baseline_vector$ga_sen_doc_fte + (ga_sen_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$return_doctor_gain)
    return_ga_jun_doc <- baseline_vector$ga_jun_doc_fte + (ga_jun_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$return_doctor_gain)
    
    ##calculate spare capacity under this increased supply
    return_staff[4] <- (return_cc_nurses * covid_vector$nurse_covid_sickness - cc_patients/baseline_vector$cc_bed_nurse) 
    return_staff[5] <- ((return_ga_nurses * covid_vector$nurse_covid_sickness) - ga_patients/baseline_vector$ga_bed_nurse) 
    return_staff[6] <- ((return_cc_sen_doc * covid_vector$doc_covid_sickness) - cc_patients/baseline_vector$cc_bed_sen_doc)
    return_staff[7] <- ((return_cc_jun_doc * covid_vector$doc_covid_sickness) - cc_patients/baseline_vector$cc_bed_jun_doc)
    return_staff[8] <- ((return_ga_sen_doc * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_sen_doc)
    return_staff[9] <- ((return_ga_jun_doc * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_jun_doc) 
    
    ##record the increase in resources for the future combination scenarios
    return_gains[4] <- (cc_ga_nurse_split * baseline_vector$headcount_to_fte * intervention_vector$return_nurse_gain)
    return_gains[5] <- ((1- cc_ga_nurse_split) * baseline_vector$headcount_to_fte * intervention_vector$return_nurse_gain)
    return_gains[6] <- (cc_sen_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$return_doctor_gain)
    return_gains[7] <- (cc_jun_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$return_doctor_gain)
    return_gains[8] <- (ga_sen_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$return_doctor_gain)
    return_gains[9] <- (ga_jun_doc_prop * baseline_vector$headcount_to_fte * intervention_vector$return_doctor_gain)
    
    
    ##DEPLOYMENT OF STUDENTS
    
    student_deploy <- baseline;  student_deploy_gains <- baseline_gains
    
    ##increase in staff numbers
    student_ga_nurse <- baseline_vector$ga_nurses_fte + (intervention_vector$student_nurse * baseline_vector$headcount_to_fte)
    student_ga_jun_doc <- baseline_vector$ga_jun_doc_fte + (intervention_vector$student_doc * baseline_vector$headcount_to_fte)
    
    ##calculate spare capacity under this increased supply
    student_deploy[5] <- (student_ga_nurse * covid_vector$nurse_covid_sickness) - ga_patients/baseline_vector$ga_bed_nurse
    student_deploy[9] <- (student_ga_jun_doc * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_jun_doc
    
    ##record the increase in resources for the future combination scenarios
    student_deploy_gains[5] <- (intervention_vector$student_nurse * baseline_vector$headcount_to_fte)
    student_deploy_gains[9] <- (intervention_vector$student_doc * baseline_vector$headcount_to_fte)
    
    
    
    ###Now combine all of this information from the individual scenarios before calculating combination scenarios
    
    gains_df <- rbind.data.frame(field_hosp_gains, private_res_gains, return_gains,student_deploy_gains)
    
    spare_capacity_df <- rbind.data.frame(baseline,field_hosp,private_res, return_staff, student_deploy)
    colnames(spare_capacity_df) <- c("CC beds","G&A beds","Ventilators","CC nurses","G&A nurses","CC senior doctors",
                                     "CC junior doctors","G&A senior doctors","G&A junior doctors")
    spare_capacity_df$intervention <- c("Baseline & electives cancelled","Field hospitals","Private sector resources",
                                        "Return of former staff","Student deployment")
    
    
    ###ALL INTERVENTION SCENARIO
    
    #calculate capacity under these interventions
    combo_all_effects <- colSums(gains_df[1:4,])
    #get the baseline when this is over
    combo_all_and_baseline <- combo_all_effects + c(baseline_vector$total_cc_beds, baseline_vector$total_ga_beds, 
                                                    baseline_vector$total_vents,baseline_vector$cc_nurses_fte,
                                                    baseline_vector$ga_nurses_fte, baseline_vector$cc_sen_doc_fte,
                                                    baseline_vector$cc_jun_doc_fte, baseline_vector$ga_sen_doc_fte, 
                                                    baseline_vector$ga_jun_doc_fte)
    
    #calculate capacity with the new resource amounts
    combo_all <- baseline
    combo_all[1] <- (combo_all_and_baseline[1] - cc_patients) 
    combo_all[2] <- (combo_all_and_baseline[2] - ga_patients) 
    combo_all[3] <- (combo_all_and_baseline[3] - ventilator_need) 
    combo_all[4] <- ((combo_all_and_baseline[4] * covid_vector$nurse_covid_sickness) - cc_patients/baseline_vector$cc_bed_nurse) 
    combo_all[5] <- ((combo_all_and_baseline[5] * covid_vector$nurse_covid_sickness) - ga_patients/baseline_vector$ga_bed_nurse) 
    combo_all[6] <- ((combo_all_and_baseline[6] * covid_vector$doc_covid_sickness) - cc_patients/baseline_vector$cc_bed_sen_doc)
    combo_all[7] <- ((combo_all_and_baseline[7] * covid_vector$doc_covid_sickness) - cc_patients/baseline_vector$cc_bed_jun_doc)
    combo_all[8] <- ((combo_all_and_baseline[8] * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_sen_doc) 
    combo_all[9] <- ((combo_all_and_baseline[9] * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_jun_doc)
      
    #format for combining with the previous output
    combo_all <- as.data.frame(t(combo_all))
    colnames(combo_all) <-  c("CC beds","G&A beds","Ventilators","CC nurses","G&A nurses","CC senior doctors","CC junior doctors",
                            "G&A senior doctors","G&A junior doctors")
    combo_all$intervention <- "Combination (All)"
    #combine with previous output  
    spare_capacity_df <- rbind.data.frame(spare_capacity_df, combo_all)
    
      
    ###SUSTAINABLE INTERVENTION SCENARIO (field hospitals & student deployment)
    
    #calculate capacity under these interventions
    combo_sus_effects <- colSums(gains_df[c(1,4),])
    #get the baseline when this is over
    combo_sus_and_baseline <- combo_sus_effects + c(baseline_vector$total_cc_beds, baseline_vector$total_ga_beds, 
                                                    baseline_vector$total_vents,baseline_vector$cc_nurses_fte,
                                                    baseline_vector$ga_nurses_fte, baseline_vector$cc_sen_doc_fte,
                                                    baseline_vector$cc_jun_doc_fte, baseline_vector$ga_sen_doc_fte, 
                                                    baseline_vector$ga_jun_doc_fte)
    
    #calculate capacity with the new resource amounts
    combo_sus <- baseline
    combo_sus[1] <- (combo_sus_and_baseline[1] - cc_patients) 
    combo_sus[2] <- (combo_sus_and_baseline[2] - ga_patients) 
    combo_sus[3] <- (combo_sus_and_baseline[3] - ventilator_need) 
    combo_sus[4] <- ((combo_sus_and_baseline[4] * covid_vector$nurse_covid_sickness) - cc_patients/baseline_vector$cc_bed_nurse) 
    combo_sus[5] <- ((combo_sus_and_baseline[5] * covid_vector$nurse_covid_sickness) - ga_patients/baseline_vector$ga_bed_nurse) 
    combo_sus[6] <- ((combo_sus_and_baseline[6] * covid_vector$doc_covid_sickness) - cc_patients/baseline_vector$cc_bed_sen_doc)
    combo_sus[7] <- ((combo_sus_and_baseline[7] * covid_vector$doc_covid_sickness) - cc_patients/baseline_vector$cc_bed_jun_doc)
    combo_sus[8] <- ((combo_sus_and_baseline[8] * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_sen_doc) 
    combo_sus[9] <- ((combo_sus_and_baseline[9] * covid_vector$doc_covid_sickness) - ga_patients/baseline_vector$ga_bed_jun_doc)
    
    #format for combining with the previous output
    combo_sus <- as.data.frame(t(combo_sus))
    colnames(combo_sus) <-  c("CC beds","G&A beds","Ventilators","CC nurses","G&A nurses","CC senior doctors","CC junior doctors",
                          "G&A senior doctors","G&A junior doctors")
    combo_sus$intervention <- "Combination (All)"
    #combine with previous output  
    spare_capacity_df <- rbind.data.frame(spare_capacity_df, combo_sus)

    ##now record the patient numbers for which this was calculated
    spare_capacity_df$electives_ga <- non_covid_ga
    spare_capacity_df$electives_cc <- non_covid_ga
    spare_capacity <- rbind.data.frame(spare_capacity, spare_capacity_df)
   
  }#end of iteration for this number of non-covid patients
  
  #rearrange order for output
  spare_capacity <- spare_capacity[,c(10,1:9,11:12)]
  return(spare_capacity)
  
}

###now we can loop this function over different covid patient numbers to see how many electives we can introduce under varying numbers
#vary in intervals of 10% from 0 to the maximum observed numbers in G&A and CC
perc_covid <- c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0)
all_data <- c()

for(i in 1:length(perc_covid)){
  
  covid_data_loop <- covid_data
  #adjust the number of covid patients based on the percentage in the iteration
  covid_data_loop$covid_cc_num <- perc_covid[i]*covid_data$covid_cc_num
  covid_data_loop$covid_ga_num <- perc_covid[i]*covid_data$covid_ga_num
  
  #use the function above to determine capacity under the different numbers of elective patients with this as the fixed covid patient number
  output_loop <- hospital_capacity_electives_surgery(baseline_data,covid_data_loop,intervention_data,ga_non_covid_surgery = seq(52982,99569,500))
  
  #record the number of covid patients that each output of the function correspond to
  output_loop$covidprop <- perc_covid[i] 
  output_loop$covid_cc <- perc_covid[i]*3100
  output_loop$covid_ga <- perc_covid[i]*15700
  
  all_data <- rbind(all_data,output_loop)

}

###all_data can then be used to analyse spare capacity under different scenarios and number of COVID-19 and non-COVID-19 patients
