perform_eda <- function(data) {
 dir.create("output/results",recursive=TRUE,showWarnings=FALSE);dir.create("output/tables",recursive=TRUE,showWarnings=FALSE)
 y<-data$Year[!is.na(data$Year)];s<-data$Global_Sales[!is.na(data$Global_Sales)]
 o<-data.frame(records=nrow(data),columns=ncol(data),unique_games=dplyr::n_distinct(data$Name),platforms=dplyr::n_distinct(data$Platform,na.rm=TRUE),genres=dplyr::n_distinct(data$Genre,na.rm=TRUE),publishers=dplyr::n_distinct(data$Publisher[data$Publisher!="Unknown"]),first_year=if(length(y))min(y) else NA_real_,last_year=if(length(y))max(y)else NA_real_,global_sales_total=sum(s),global_sales_mean=if(length(s))mean(s)else NA_real_,global_sales_median=if(length(s))median(s)else NA_real_)
 m<-data.frame(column=names(data),missing_count=colSums(is.na(data)));readr::write_csv(o,"output/tables/dataset_overview.csv");readr::write_csv(m,"output/tables/missing_values.csv")
 z<-c("VIDEO GAME SALES: EDA SUMMARY",strrep("=",31),capture.output(print(o)),"Missing values:",capture.output(print(m,row.names=FALSE)),"Basic summary:",capture.output(print(summary(data))))
 writeLines(z,"output/results/eda_summary.txt");cat(paste(z[1:5],collapse="\n"),"\n");invisible(list(overview=o,missing=m))
}
