#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_project = function(){

  gsub('.+\\/(.+)$', '\\1', rstudioapi::getActiveProject())

}
