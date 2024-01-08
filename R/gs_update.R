#' Google Sheet4를 사용해서 sheet 정보를 가져옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
gs_update = function(df_is, sheet_is, project_is, is_write = F){

  # DataFrame이 없음

  if(is.null(df_is)){

    return(cat(crayon::green('√'),'No DataFrame found :(\n'))

  }

  # Sheet 지정 값이 없음
  if(is.null(sheet_is) || sheet_is == ''){

    return(cat(crayon::green('√'),'No Sheet Information found :(\n'))
  }

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

  # Env 확인용 플래그
  code_gs_exist = sum(ls(envir = .GlobalEnv) == 'code_gs')

  # Master Key가 없으므로 gs_master 실행을 요청
  if(code_gs_exist == 0){

    gs_master(project_is)

  }

  # loader에 프로젝트 정보가 없을 경우
  if(sum(code_gs$loader$project_id == project_is) == 0){

    gs_master(project_is)

  }
  # Meta 정보가 없으므로 gs_master 실행을 요청

  # Env Loader에서 필요한 ss 정보 검색
  loader_wanted = code_gs$loader %>% filter(project_id == project_is & sheets == sheet_is)


  # 비어 있는 Sheet인지 확인 (비어 있을 경우 append가 아니라 강제 write로 시작)
  is_empty = googlesheets4::read_sheet(ss=loader_wanted$ss, sheet=sheet_is, range = 'A1:A2')
  is_empty = nrow(is_empty) == 0


  # GoogleSheet에 쓰기 시작
  if(is_empty == T){ # 비어 있는 Sheet일 경우 강제로 Write

    googlesheets4::sheet_write(data=df_is,ss=loader_wanted$ss,sheet=loader_wanted$sheets)
    cat(crayon::green('√'),crayon::yellow(project_is),'Sheet',crayon::red(sheet_is),'Written Initially:)\n')

  } else if(is_write == F){ # Append

    googlesheets4::sheet_append(ss=loader_wanted$ss, data=df_is, sheet = loader_wanted$sheets)
    cat(crayon::green('√'),crayon::yellow(project_is),'Sheet',crayon::red(sheet_is),'Appended :)\n')

  } else { # Write

    googlesheets4::sheet_write(data=df_is,ss=loader_wanted$ss,sheet=loader_wanted$sheets)
    cat(crayon::green('√'),crayon::yellow(project_is),'Sheet',crayon::red(sheet_is),'Written :)\n')

  }

}
