#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
where_are_you = function(){

  # getlib 이 없을 경우 설치
  if(sum(rownames(installed.packages()) == 'getlib') == 0){

    remotes::install_github('aworklab/getlib')

  }
  # 기본 함수 호출
  getlib::get_lib()

  loc_name = gs4load::gs_loader('dir_project', 'META') %>%
    filter(com_name == gs4load::get_com()) %>% pull(loc_name)

  print(sprintf('You are now in %s', loc_name))

  }
