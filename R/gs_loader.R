#' Google Sheet4를 사용해서 sheet 정보를 가져옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
gs_loader = function(sheet_is = 'loader', project_is = 'RRS'){

  # getlib 이 없을 경우 설치
  if(sum(rownames(installed.packages()) == 'getlib') == 0){

    remotes::install_github('aworklab/getlib')

  }

 # 기본 함수 호출

  tibble_exist = sum((.packages()) == 'tibble')

  if(tibble_exist == 0){

    library(getlib)
    get_lib()

  }


  # Env 확인용 플래그
  code_gs_exist = sum(ls(envir = .GlobalEnv) == 'code_gs')

  # Sheet 지정 값이 없음
  if(is.null(sheet_is) || sheet_is == ''){

    return(cat(crayon::green('√'),'No Sheet Information found :(\n'))
  }

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


  # 필요한 정보를 찾았을 경우 GS에서 값을 호출
  if(nrow(loader_wanted) != 0){

    loader = googlesheets4::read_sheet(loader_wanted$ss, loader_wanted$sheets)

    # print(sprintf('[%s]Sheet %s loaded', project_is, sheet_is))
    cat(crayon::green('√'),crayon::yellow(project_is),'Sheet',crayon::red(sheet_is),'loaded :)\n')

    return(loader)

    # Sheet 정보가 없을 경우 중지
  } else {

    return(cat(crayon::green('√'),crayon::yellow(project_is),'Sheet',crayon::red(sheet_is),'is not found :(\n'))
    # return(print(sprintf('[%s]Sheet %s is not found', project_is, sheet_is)))

  }

}
