# remotes::install_github('aworklab/gs4load', upgrade = c('never'), force = T)
pacman::p_load(gs4load)
get_lib()


# 0. meta -----------------------------------------------------------------

t_prefix = 'sf_cbt'

# 1. SF Qlite - Mysql -----------------------------------------------------


db_info = gs_loader('bs_db','META') %>% filter(com_is == get_com()) %>%
  filter(prefix == t_prefix)

con_q = get_con(t_prefix,'sql')
list_tbl = dbListTables(con_q)
list_tbl = list_tbl[str_detect(list_tbl,'db|mix')]
list_tbl = tibble(table = list_tbl)
dbDisconnect(con_q)

# ** Import from mysql ----------------------------------------------------

work_tbl = list_tbl
work_tbl = work_tbl %>% filter(table == 'mix_people')

i = 1

for(i in 1:nrow(work_tbl)){

  # `bk_common`.`cs_tester`

  .x = work_tbl$table[[i]];.x

  # DB 정보

  sqlite_name = db_info$dbname
  table_name = .x

  create_name = glue('`sf_cbt`.`{table_name}`')
  print(create_name)
  # Con 생성
  con_q = get_con(t_prefix, 'sql')
  con_my = get_con(t_prefix, 'my')

  # SQLite
  query = glue('select * from {table_name}')
  temp = dbGetQuery(con_q, query) %>% as_tibble()

  temp = str2dt(temp)

  if(nrow(temp) != 0){

    info_create = get_mysql_schema(temp, create_name)

    dbSendQuery(con_my, info_create$query)

    dbWriteTable(con_my,
                 table_name,
                 temp,
                 append = T, row.names = F)

  }
  # MYsql Create Query 생성

  dbDisconnect(con_my)
  dbDisconnect(con_q)

}

# 속도 테스트  -----------------------------------------------------------------

con_my = get_con(t_prefix,'my')
con_q = get_con(t_prefix,'sql')

query_01 = 'select * from mix_event'
s1 = Sys.time()
my = dbGetQuery(con_my , query_01)
e1 = Sys.time()
s2 = Sys.time()
sq = dbGetQuery(con_q, query_01)
e2 = Sys.time()

cat(sprintf('
        My : %s
        Light : %s
        ', e1-s1,e2-s2))


query_02 = 'select * from mix_event where time >= 1702463043 and time <=1702479123'
s1 = Sys.time()
my = dbGetQuery(con_my , query_02)
e1 = Sys.time()
s2 = Sys.time()
sq = dbGetQuery(con_q, query_02)
e2 = Sys.time()

cat(sprintf('
        My : %s
        Light : %s
        ', e1-s1,e2-s2))

