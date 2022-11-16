#' @title Make unique and complete
#' @export
#' @author Julie W. Turner
#' 
make_unique_complete <- function(DT, id, datetime, long, lat) {
  na.omit(unique(DT, by = c(id, datetime)),
          cols = c(long, lat, datetime))
}
