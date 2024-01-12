#' DB Conn 확보
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_con = function(prefix_is, db_is = 'sql'){

  # Prefix가 없을 경우 종료
  if(sum(!is.null(prefix_is)) == 0){
    return(cat(green('√'),'No Prefix Information...\n'))
  }

  # db_is 값이 다를 경우 종료

  if(!db_is %in% c('sql','my','ms')){
    return(cat(green('√'),'No Valid DB Name...\n'))
  }

  # DB 별 상세

  db_type = tibble(db_is = c('sql','my','ms'),
         db_type = c('SQLite','MYSQL','MSSQL'))

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

    db_set_exist = ifelse(sum(ls(code_gs) == 'db_set') == 1, 1, 0)

  } else {

    db_set_exist = 0

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
    meta_key = code_gs$loader_master$key_id[code_gs$loader_master$project_id == 'META']
    loader = googlesheets4::read_sheet(meta_key, 'loader')
    assign('loader', loader, envir = code_gs)

  }

  if(db_set_exist == 0){

    googlesheets4::gs4_auth(path = path_key)
    gs_sys_master = '1CA-1Z9wPWVEXcxGcSZ9_dUXeii_uj9bQDnCgkuBylPc'
    sheet_key = code_gs$loader$ss[code_gs$loader$sheets == 'gs4test']
    db_set = googlesheets4::read_sheet(sheet_key, 'bs_db') %>% filter(com_is == where_is)
    assign('db_set', db_set, envir = code_gs)

  } else {

    db_set = code_gs$db_set

  }

  # SQLite, MSSQL, MYSQl에 따라 다르게 잡음

  if(db_is == 'sql'){ # SQLite

    # 연결할 DB 네임 설정
    dbname_is = db_set$dbname[which(db_set$prefix == prefix_is)]

    # 연결 설정
    conn = DBI::dbConnect(RSQLite::SQLite(), dbname = dbname_is)

  } else if (db_is == 'ms'){ # MSSQL

    # 연결할 DB 네임 설정
    dbname_is = str_split(db_set$mssql_db[which(db_set$prefix == prefix_is) ], '@')[[1]]

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

    # 연결할 DB 네임 설정
    dbname_is = str_split(db_set$mysql_db[which(db_set$prefix == prefix_is) ], '@')[[1]]
    # schema_is = db_set$t_name[which(db_set$prefix == prefix_is) ]
    schema_is = prefix_is

    conn = DBI::dbConnect(
      RMySQL::MySQL(),
      dbname = schema_is,
      host = dbname_is[1],
      user = dbname_is[2],
      password = keyring::key_get(dbname_is[3]),
      port = 3306,
      local_infile = T
    ) -> con


  } # Conneciton 확보 종료

  db_type = db_type$db_type[which(db_type$db_is == db_is)]

  cat(green('√'),blue(db_type),
      'Connection with',silver('DB'),red(prefix_is),'Established !!!\n')

  return(conn)

}
