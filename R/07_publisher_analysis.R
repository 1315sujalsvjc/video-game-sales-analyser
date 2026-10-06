analyze_publishers <- function(data) {
 x<-data|>dplyr::filter(!is.na(Publisher),Publisher!="Unknown",!is.na(Global_Sales))|>dplyr::group_by(Publisher)|>dplyr::summarise(total_sales=sum(Global_Sales),game_count=dplyr::n(),average_sales=mean(Global_Sales),.groups="drop")|>dplyr::arrange(dplyr::desc(total_sales));t<-dplyr::slice_head(x,n=10)
 readr::write_csv(x,"output/tables/publisher_sales.csv");readr::write_csv(t,"output/tables/top_publishers.csv");if(nrow(t))cat("Top publisher:",t$Publisher[1],"\n");invisible(list(publisher_sales=x,top_publishers=t))
}
