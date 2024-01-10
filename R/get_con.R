#' DB Conn 확보
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_con = function(prefix_is, db_is = 'sql'){

  prefix_is = 'sf_cbt'
  sql_is = 'sqlite'

  # Prefix가 없을 경우 종료
  if(sum(!is.null(prefix_is)) == 0){
    return(cat(green('√'),'No Prefix Information...\n'))
  }

  # db_is 값이 다를 경우 종료

  if(!db_is %in% c('sql','my','ms')){
    return(cat(green('√'),'No Valid DB Name...\n'))
  }

  # GS4 KEY 설정
  path_key = keyring::key_get('gs4')

  # 컴퓨터 이름 판별
  where_is = Sys.info()['nodename']

  # 판별 조건 확인

  code_gs_exist = sum(ls(envir = .GlobalEnv) == 'code_gs')


  if(code_gs_exist == 1){

    master_exist = ifelse(sum(ls(code_gs) == 'loader_master') == 1, 1, 0)

    if(master_exist == 1){

      master_exist = ifelse(sum(code_gs$loader_master$project_id == 'META') == 1, 1, 0)

    }

  } else {

    master_exist = 0

  }

  if(master_exist == 1){

    loader_exist = ifelse(sum(code_gs$loader$sheets == 'bs_db') == 1, 1, 0)

  } else {

    loader_exist = 0

  }

  if(master_exist == 1){

    db_info_exist = ifelse(sum(ls(code_gs) == 'db_info') == 1, 1, 0)

  } else {

    db_info_exist = 0

  }

  # 조건에 따라 추가 정보 호출
  if(code_gs_exist == 0){

    assign('code_gs', new.env(), envir = .GlobalEnv)

  }

  if(master_exist == 0){

    googlesheets4::gs4_auth(path = path_key)
    gs_sys_master = '1CA-1Z9wPWVEXcxGcSZ9_dUXeii_uj9bQDnCgkuBylPc'
    loader_master = googlesheets4::read_sheet(gs_sys_master, 'loader')
    assign('loader_master', loader_master, envir = code_gs)
  }

  if(loader_exist == 0){

    googlesheets4::gs4_auth(path = path_key)
    gs_sys_master = '1CA-1Z9wPWVEXcxGcSZ9_dUXeii_uj9bQDnCgkuBylPc'
    meta_key = loader_master$key_id[loader_master$project_id == 'META']
    loader = googlesheets4::read_sheet(meta_key, 'loader')
    assign('loader', loader, envir = code_gs)

  }

  if(db_info_exist == 0){

    googlesheets4::gs4_auth(path = path_key)
    gs_sys_master = '1CA-1Z9wPWVEXcxGcSZ9_dUXeii_uj9bQDnCgkuBylPc'
    sheet_key = loader$ss[loader$sheets == 'gs4test']
    db_info = googlesheets4::read_sheet(sheet_key, 'bs_db') %>% filter(com_is == where_is)
    assign('db_info', db_info, envir = code_gs)

  } else {

    db_info = code_gs$db_info

  }

  # SQLite, MSSQL, MYSQl에 따라 다르게 잡음

  if(db_is == 'sql'){ # SQLite

    # 연결할 DB 네임 설정
    dbname_is = db_info$dbname[which(db_info$prefix == prefix_is)]

    # 연결 설정
    conn = DBI::dbConnect(RSQLite::SQLite(), dbname = dbname_is)

  } else if (db_is == 'ms'){ # MSSQL

    # 연결할 DB 네임 설정
    dbname_is = str_split(db_info$mssql_db[which(db_info$prefix == prefix_is) ], '@')[[1]]

    # 연결 설정

    conn = DBI::dbConnect(odbc::odbc(),
                         Driver = 'SQL Server',
                         Server = dbname_is[1],
                         # Server = 'CH-LDB1-S1',
                         Database = dbname_is[2],
                         Uid = 'an_sungwook', # DB User
                         Pwd = key_get('mssql'), # DB Pass
                         Port = 1433)

  } else if (db_is == 'my'){

    return(cat(green('√'),'MY SQL is',yellow('Under Construction'),'...\n'))

  } # Conneciton 확보 종료

  cat(green('√'),'Connection with',silver('DB'),red(prefix_is),'Established !!!\n')

  return(conn)

}