#' Google Sheet4를 사용해서 sheet 정보를 가져옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
gs_update = function(the_sheet = '', info = NULL){


  # 기본 함수 호출

  tibble_exist = sum((.packages()) == 'tibble')

  if(tibble_exist == 0){

    gs4load::get_lib()

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

  # check loader

  if(!'loader' %in% ls(code_gs)){

    gs_sys = '1CgrVo4NdeYvrC9Z1dwHJxDifE-bNudVlmaxcg-67hnI'
    loader = googlesheets4::read_sheet(gs_sys, 'loader', col_types = c('cccc'))
    assign('loader', loader, envir = code_gs)

  }

  # env에 해당 값 있는지 확인

  if(sheet == ''){

    return(print('no sheet'))

  } else if (is.null(info)){

    return(print('nothing to update'))

  } else if (nrow(info) == 0){

    return(print('nrow is 0'))

  }

  t_info = dplyr::filter(code_gs$loader, sheets == the_sheet)

  googlesheets4::sheet_append(t_info$ss, info, sheet = the_sheet)

  print(sprintf('appended in %s', t_info$sheets))

}
