#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_lib = function(){

  pacman::p_load(tidyverse, lubridate, glue, fs, data.table,pak)
  pacman::p_load(googlesheets4,dtplyr)
  pacman::p_load(plotly, rjson, jsonlite)
  pacman::p_load(pbapply)
  pacman::p_load(rvest,httr)

}
