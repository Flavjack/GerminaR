#' Multiple comparison test
#'
#' @description Function analysis of variance for summary data.
#' @param aov lm o aov result function.
#' @param comp treatments will be compared.
#' @param type method for made comparison analysis: c("snk", "tukey", "duncan").
#' @param sig significance level. Default 0.05
#' @return Table with complete data for graphics
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @import dplyr 
#' @importFrom tibble rownames_to_column
#' @importFrom purrr pluck
#' @importFrom stats fitted resid rstandard
#' @export
#' 
#' @examples  
#' 
#' \dontrun{
#' 
#' library(GerminaR)
#' library(dplyr)
#' data <- prosopis %>% mutate(across(c(nacl, temp, rep), as.factor))
#' smr <- ger_summary(SeedN = "seeds", evalName = "D", data = data)
#' 
#' aov <- aov(grp ~ nacl*temp, smr)
#' 
#' mc <- ger_testcomp(aov = aov
#'                    , comp <- c("nacl", "temp")
#'                    )
#' } 

ger_testcomp <- function( aov, comp, type = "snk", sig = 0.05){
  
  if (type == "snk"){
    
    mc <- SNK.test(
      y = aov
      , trt = comp
      , alpha = sig
    )
    
  } else if (type == "tukey") {
    
    mc <- HSD.test(
      y = aov
      , trt = comp
      , alpha = sig
    )
    
  } else if (type == "duncan") {
    
    mc <- duncan.test(
      y = aov
      , trt = comp
      , alpha = sig
    )
  }
  
  tb_mc <- merge(
    mc %>% pluck("means") %>% rownames_to_column("treatments")
    , mc %>% pluck("groups") %>% rownames_to_column("treatments")
    , all = TRUE) %>%
    rename_with(tolower) %>%
    mutate(ste = .data$std/sqrt(.data$r), .after = .data$r) %>%
    select(!c(.data$q25, .data$q50, .data$q75)) %>%
    rename("sig" = .data$groups)
  
  if ( length(comp) <= 2 ) {
    
    tb_mc <- tb_mc %>%
      tidyr::separate(.data$treatments, {{comp}}, sep = ":")
    
  }
  
  smr_stat <- mc %>%
    pluck("statistics") %>%
    merge(mc$parameters, .) 
  
# diagnostic plots --------------------------------------------------------

  diagplot <- function( model ) {
    
    p1 <- ggplot(model , aes(rstandard(model))) +
      geom_histogram(bins = 30) +
      xlab("Residuals") +
      ylab("Frequency") +
      ggtitle("Frequency vs Residuals") +
      theme_bw()
    
    p2 <- ggplot(model, aes(fitted(model), resid(model))) +
      geom_point() +
      stat_smooth(method="loess", formula = 'y ~ x') +
      geom_hline(yintercept=0, col="red", linetype="dashed") +
      xlab("Fitted values") +
      ylab("Residuals") +
      ggtitle("Residual vs Fitted") +
      theme_bw()
    
    p3 <- ggplot(model, aes(sample = rstandard(model))) +
      geom_qq() +
      stat_qq_line() +
      xlab("Theoretical Quantiles") +
      ylab("Standardized Residuals") +
      ggtitle("Normal Q-Q") +
      theme_bw()
    
    p4 <- ggplot(model, aes(fitted(model), sqrt(abs(resid(model))))) +
      geom_point(na.rm=TRUE) +
      stat_smooth(method="loess", na.rm = TRUE, formula = 'y ~ x') +
      xlab("Fitted Value") +
      ylab(expression(sqrt("|Standardized residuals|"))) +
      ggtitle("Scale-Location") +
      theme_bw()
    
    return(list( freq = p1, qqnorm = p2, resid = p3, sresid = p4))
    
  }
  
  diagplot <- diagplot(model = aov)

# result ------------------------------------------------------------------

mean_comparison <- list(
    table = tb_mc
    , stats = smr_stat
    , diagplot = diagplot
  )
    
}

