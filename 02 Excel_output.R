
dataset_names<- list('National' = Ph1st_national, 
                     'Regional' = ph1st_regional, 
                     'ICB' = get_pha1st_table_2b()
)


openxlsx::write.xlsx(dataset_names, file = paste0('Outputs/Pharm1st_OptIn_', Sys.Date(), '.xlsx')) 



