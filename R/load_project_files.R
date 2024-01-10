#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
load_project_files = function(){

  # 기본 함수 호출
  # gs4load::get_lib()

  list_project = gs4load::gs_loader('project_file','META')

  com_is = gs4load::get_com()
  project_is = gs4load::get_project()

  cat(crayon::green('√'),'Current Computer is',crayon::red(com_is),'\n')
  cat(crayon::green('√'),'Current Project is',crayon::blue(project_is),'\n')

  target_list = list_project |> dplyr::filter(com_name == com_is & project_name == project_is)

  if(nrow(target_list) == 0){

    return(cat(crayon::red('∀'),'There is No project Information\n',
               crayon::red('∀'),crayon::silver('Task Aborted...\n')))

  }

  target_list = target_list |> dplyr::mutate(dir_is = sprintf('%s/%s',dir_name,file)) %>% pull(dir_is)

  lapply(target_list, function(x) rstudioapi::navigateToFile(x))

  cat(crayon::green('√'),'Project',crayon::blue(project_is),'file is loaded\n')

}
