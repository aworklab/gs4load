#' Google Sheet4를 사용해서 sheet 정보를 가져옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
gs_master = function(project_is = NULL){

  # 기본 함수 호출

  tibble_exist = sum((.packages()) == 'tibble')

  if(tibble_exist == 0){

    library(getlib)
    get_lib()

  }

  # API 설정
  call_api = function() {

    path_key_url = keyring::key_get('gs4')
    googlesheets4::gs4_auth(path = path_key_url)

  }

  call_api()

  # loader master key value

  gs_sys_master = '1CA-1Z9wPWVEXcxGcSZ9_dUXeii_uj9bQDnCgkuBylPc'
  loader_master = googlesheets4::read_sheet(gs_sys_master, 'loader')

  # code_gs Env 생성

  if(!('code_gs' %in% ls(envir = .GlobalEnv))){

    assign('code_gs', new.env(), envir = .GlobalEnv)

  }

  # 체크용 조건 플래그
  project_exist = sum(!is.null(project_is))
  code_gs_exist = sum(ls(envir = .GlobalEnv) == 'code_gs')

  if(project_exist == 1 & code_gs_exist == 1){

    # Project 명이 있고, code_gs가 있을 때, Project 정보를 불러와서 Merge함
    dplyr::bind_rows(
      loader_master %>% filter(project_id == project_is),
      code_gs$loader_master
    ) %>% unique() -> loader_master

  } else if(project_exist == 1 & code_gs_exist == 0){

    # Project명이 있고, code_gs가 없을 때, 지정한 프로젝트만 사용함
    loader_master = loader_master %>% filter(project_id == project_is)

  }

  assign('loader_master', loader_master, envir = code_gs)

  # Load Full key list

  loader_master %>%
    filter(!is.na(key_id)) %>%
    mutate(fit = map(key_id,
                     ~{
                       googlesheets4::read_sheet(.x, 'loader', col_types = c('cccc'))
                     })) -> loader

  loader %>% select(-key_id) %>%
    unnest(fit) -> loader

  assign('loader', loader, envir = code_gs)

  # Assign Project name
  key_name = paste0(unique(loader$project_id), collapse = ', ')

  # print(sprintf('Loader Key id assiged : %s', paste0(unique(loader$project_id), collapse = ', ')))
  print(cat(crayon::green('√'),'Loader Key id Assigned :',crayon::red(key_name),'\n'))
}
