#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_lib = function(){

  pacman::p_load(tidyverse, lubridate, glue, fs, data.table)
  pacman::p_load(googlesheets4)
  pacman::p_load(plotly)
}
