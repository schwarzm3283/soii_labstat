
library(data.table)
library(DBI)

is_alldata<-fread("https://download.bls.gov/pub/time.series/is/is.data.1.AllData")

#connection to postgres
pw <- {
  "mikeS3283!!05"
}
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "soii_ors",
                 host = "localhost", port = 5432,
                 user = "schwarzm3283", password = pw)
rm(pw)


test <- left_join(is_alldata, is_series, by ="series_id")

list_df <- mget(ls()[1:length(ls())])

#write to data folder
lapply(seq_along(list_df),
       function(i) dbWriteTable(con,
                                paste0(names(list_df)[i]),
                                list_df[[i]], 
                               row.names = FALSE,
                               overwrite = TRUE
                               ))
#set wd back to main

for (i in 1:length(list_df)) {
  dbWriteTable(con,  list_df[i], get0(list_df[i]), overwrite = TRUE, row.names=FALSE)
}

#write table
dbWriteTable(pool, "si_alldata", si_alldata, row.names=FALSE, overwrite = TRUE)

list_df <- lapply(ls(), function(x) if (class(get(x)) == "data.frame") get(x))
list_df <- list_df[!sapply(list_df, is.null)] 



src_pool(pool) %>% tbl("is_area") %>%
  dplyr::filter(area_text == local(state)) %>%
  select(area_code) %>% explain()

#---------sample query


# SELECT i.industry_text, ad.value, ad.industry_code
# FROM is_alldata ad
# LEFT JOIN is_industry i ON ad.industry_code = i.industry_code
# WHERE ad.area_code = 101 and ad.industry_code = 'GP1CON' 
