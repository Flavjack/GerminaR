
#' Summary of germination variables
#' @description This function made a data table with the values of germination variables
#' @param data Data with the germination avaliation
#' @return Data frame with the values of germination variables
#' @export
Summary_Ger <- function(data){
  eval_days <- select(data, starts_with("D"))
  SDN <- data$SDN
  sm <- data %>% select(-starts_with("D")) %>% 
    mutate(
      GRS = GRS(eval_days), 
      GRP = GRP(SDN,eval_days),
      ASG = ASG(SDN,eval_days),
      MGT = MGT(eval_days),
      CVG = CVG(eval_days),
      MGR = MGR(eval_days),
      GRU = GRU(eval_days),
      GSI = GSI(eval_days),
      VGT = VGT(eval_days),
      SDG = SDG(eval_days),
      CoVG = CoVG(eval_days)
    ) 
}