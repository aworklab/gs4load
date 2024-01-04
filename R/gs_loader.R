#' Google Sheet4를 사용해서 sheet 정보를 가져옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
gs_loader = function(sheet_is = 'loader', project_is = 'RRS'){

  # Master Key가 없으므로 gs_master 실행을 요청
  tryCatch(
    {length(ls(code_gs))},
    error = function(e){
      gs_master()
    }
  )

  if(length(ls(code_gs)) == 0){

    gs_master()

  }

  # Sheet 지정 값이 없음
  if(is.null(sheet_is) || sheet_is == ''){

    return(print('No Sheet'))
  }

  # Env Loader에서 필요한 ss 정보 검색
  loader_wanted = code_gs$loader %>% filter(project_id == project_is & sheets == sheet_is)


  # 필요한 정보를 찾았을 경우 GS에서 값을 호출
  if(nrow(loader_wanted) != 0){

    loader = googlesheets4::read_sheet(loader_wanted$ss, loader_wanted$sheets)

    print(sprintf('[%s]Sheet %s loaded', project_is, sheet_is))

    return(loader)

    # Sheet 정보가 없을 경우 중지
  } else {

    return(print(sprintf('[%s]Sheet %s is not found', project_is, sheet_is)))

  }

}
