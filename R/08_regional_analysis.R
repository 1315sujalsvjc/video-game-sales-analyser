analyze_regions <- function(data) {
 l<-c(NA_Sales="North America",EU_Sales="Europe",JP_Sales="Japan",Other_Sales="Other");c<-intersect(names(l),names(data));r<-data.frame(region=unname(l[c]),sales_millions=vapply(c,function(n)sum(data[[n]],na.rm=TRUE),numeric(1)));r$contribution_percent<-if(sum(r$sales_millions)>0)100*r$sales_millions/sum(r$sales_millions)else 0
 g<-data|>dplyr::filter(!is.na(Genre),Genre!="Unknown")|>dplyr::group_by(Genre)|>dplyr::summarise(dplyr::across(dplyr::all_of(c),~sum(.x,na.rm=TRUE)),.groups="drop");g<-tidyr::pivot_longer(g,dplyr::all_of(c),names_to="region_code",values_to="sales_millions");g$region<-unname(l[g$region_code])
 readr::write_csv(r,"output/tables/regional_sales.csv");readr::write_csv(g,"output/tables/regional_genre_sales.csv");if(nrow(r))cat("Largest region:",r$region[which.max(r$sales_millions)],"\n");invisible(list(regional_sales=r,regional_genre_sales=g))
}
