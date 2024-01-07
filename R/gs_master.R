#' Google Sheet4를 사용해서 sheet 정보를 가져옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
gs_master = function(){

  # getlib 이 없을 경우 설치
  if(sum(rownames(installed.packages()) == 'getlib') == 0){

    remotes::install_github('aworklab/getlib')

  }
  # 기본 함수 호출
  if(sum((.packages()) == 'getlib') == 0){

    getlib::get_lib()

  }

  # API 설정
  call_api = function() {

    path_key_url = keyring::key_get('gs4')
    googlesheets4::gs4_auth(path = path_key_url)

  }

  call_api()

  # check env

  if(!('code_gs' %in% ls(envir = .GlobalEnv))){

    assign('code_gs', new.env(), envir = .GlobalEnv)

  }

  # loader master key value

  gs_sys_master = '1CA-1Z9wPWVEXcxGcSZ9_dUXeii_uj9bQDnCgkuBylPc'
  loader_master = googlesheets4::read_sheet(gs_sys_master, 'loader')

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

  print(sprintf('Loader Key id assiged : %s', paste0(unique(loader$project_id), collapse = ', ')))
}
