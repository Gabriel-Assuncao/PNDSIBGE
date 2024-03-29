#' Read PNDS microdata
#' @description This function reads PNDS microdata.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param microdata A text file containing microdata from PNDS survey, available on official website: (select a microdata file, according to the appropriated year, microdata folder and then, inside, data) - \samp{https://ftp.ibge.gov.br/PNDS/}.
#' @param input_txt A text file, related to the microdata, containing the input script for SAS, available on official website: (select the dictionary and input zip file, according to the appropriated year, microdata folder and then, inside, documentation) - \samp{https://ftp.ibge.gov.br/PNDS/}.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @return A tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-demografia-e-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNDSIBGE]{get_pnds} for downloading, labeling, deflating and creating survey design object for PNDS microdata.\cr \link[PNDSIBGE]{pnds_labeller} for labeling categorical variables from PNDS microdata.\cr \link[PNDSIBGE]{pnds_deflator} for adding deflator variables to PNDS microdata.\cr \link[PNDSIBGE]{pnds_design} for creating PNDS survey design object.\cr \link[PNDSIBGE]{pnds_example} for getting the path of the PNDS toy example files.
#' @examples
#' input_path <- pnds_example(path="input_example.txt")
#' data_path <- pnds_example(path="exampledata.txt")
#' pnds.df <- read_pnds(microdata=data_path, input_txt=input_path, vars=c("J007","J009"))
#' pnds.df <- pnds.df[(pnds.df$M001 == "1" & !is.na(pnds.df$M001)),]
#' pnds.df <- pnds.df[,!(names(pnds.df) %in% c("V0029", "V00291", "V00292", "V00293"))]
#' @export

read_pnds <- function(microdata, input_txt, vars = NULL) {
  message("The read_pnds function is under development and will be available soon in package PNDSIBGE.\n")
  return(NULL)
  X1 = X2 = X3 = start = end = NULL
  input <- suppressWarnings(suppressMessages({readr::read_table(input_txt, col_names=FALSE) %>% subset(substr(X1, 1, 1) == "@") %>%
    dplyr::mutate(type=ifelse(substr(X3, 1, 1) == "$","c","d"), start=as.numeric(gsub("@", "", X1)), X3=as.integer(chartr("$", " ", X3)), end=start+X3-1)}))
  if (!is.null(vars)) {
    if (any(!(vars %in% input$X2))) {
      missvar <- vars[!(vars %in% input$X2)]
      message(paste("Variables", paste(missvar, collapse=", "), "not present in microdata.\n"))
    }
    input %<>% subset(X2 %in% c("V0001", "UPA_PNDS", "ID_DOMICILIO", "V0006_PNDS", "V0015", "V0020", "V0024", "V0028", "V00281", "V00282", "V00283", "V0029", "V00291", "V00292", "V00293", "C00301", "M001", "W001", vars))
  }
  columns <- input %$% readr::fwf_positions(start, end, X2)
  data_pnds <- suppressWarnings(readr::read_fwf(microdata, columns, col_types=paste0(input$type, collapse="")))
  data_pnds <- dplyr::mutate(data_pnds, ID_DOMICILIO=paste0(data_pnds$UPA_PNDS, data_pnds$V0006_PNDS))
  data_pnds <- data_pnds[(data_pnds$V0015 == "01" & !is.na(data_pnds$V0015)),]
  return(data_pnds)
}
