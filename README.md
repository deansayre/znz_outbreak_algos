# znz_outbreak_algos

## Code analyzing performance of simple outbreak detection algorithms on routine data

The .R scripts and .Rmd files listed above analyze routine data obtained from the Zanzibar Malaria Elimination Program to establish and 
apply a gold standard definition for unexpected malaria upsurges and compare these to algorithms widely applicable to routine data in 
order to establish performance metrics for each. 

The files are to be run in the following sequence to produce all outputs. 

1) de_dupe_fuzzy.R (if desired... only a template for what was performed for deduplication; this process was handled by ZAMEP)
2) znz_explore.Rmd
3) farrington_thresholds.R
4) znz_outbreak.Rmd
5) metric_calc.Rmd (and metric_dashboard.Rmd, if desired)
6) znz_report.Rmd
7) znz_report_app9.Rmd (if desired)

Data files **NOT** shared here required to reproduce results are 

1) final_data_cleaned_De_dups.xlsx
2) censored_hf_mk_hm.xlsx (contains name changes performed during cleaning)
3) shehia_loc.xlsx
4) shehia_spell_corrected.xlsx
5) shehia_district_not_match_shp_HM.xlsx
6) deduped_unfiltered_data.xlsx
7) data_unfiltered_clean.xlsx
8) zamep_overwrite.xlsx

There are additional xlsx files imported during the process, but these are created by one of the included scripts. 

