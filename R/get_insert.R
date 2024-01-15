#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_insert = function(df, target_name){

  target_name = "wotan_delabs.setup_info"
  get_ts_name(df)
  get_char_name(df)
  get_mysql_schema(df, target_name)

}
