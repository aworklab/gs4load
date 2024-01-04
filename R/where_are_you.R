#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
where_are_you = function(){

  gs4load::get_lib()

  temp = gs4load::gs_loader('dir_project', 'META')

  loc_name = temp %>% filter(com_name == gs4load::get_com())

  print(sprintf('You are now in %s', loc_name$loc_name))

  }
