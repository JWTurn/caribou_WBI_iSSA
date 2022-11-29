#' @title calculate time since fire
#' @export
#' @author Julie W. Turner
#' 
calc_tsf <- function(DT, step.end  = T, nofire=100){
  
  
  if (isTRUE(step.end)) {
    DT[,tsf_end:= year - fires_end]
    DT[is.na(tsf_end), tsf_end:=nofire]
  }
  
  if(isFALSE(step.end)){
    DT[,tsf_start:= year - fires_start]
    DT[is.na(tsf_end), tsf_end:=nofire]
  }
  
  
}