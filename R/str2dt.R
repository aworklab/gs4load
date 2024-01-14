#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
str2dt = function(df){

  chk = gs4load::get_ts_name(df)

  if(nrow(chk)!=0){

    for(i in 1:nrow(chk)){

      work_info = chk[i, ]
      work_col = work_info$col
      work_flag = work_info$flag
      converter = ifelse(work_flag == 'ts', 'ymd_hms','ymd')
      txt = glue('df${work_col} = {converter}(df${work_col})')
      eval(parse(text = txt))

      cat(magenta('>')
          ,yellow(work_col),'is converted to',blue(work_flag),'\n')

    } # for 문 종료

  } # if문 종료


  return(df)

}
