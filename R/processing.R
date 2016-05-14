
#' Summary of germination variables
#' @description This function made a data table with the values of germination variables
#' @param data Data with the germination avaliation
#' @return Data frame with the values of germination variables
#' @export
ger_summary <- function(data){
  eval_days <- select(data, starts_with("D"))
  SDN <- data$SDN
  sm <- data %>% select(-starts_with("D")) %>% 
    mutate(
      GRS = ger_GRS(eval_days), 
      GRP = ger_GRP(SDN,eval_days),
      ASG = ger_ASG(SDN,eval_days),
      MGT = ger_MGT(eval_days),
      CVL = ger_CVL(eval_days),
      MGR = ger_MGR(eval_days),
      GRU = ger_GRU(eval_days),
      GSI = ger_GSI(eval_days),
      VGT = ger_VGT(eval_days),
      SDG = ger_SDG(eval_days),
      CVG = ger_CVG(eval_days)
    ) 
}