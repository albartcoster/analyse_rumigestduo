## data voor app Algra
packages <- c("RODBC",
              "lubridate",
              "RPostgres",
              "tidyverse",
              "dotenv",
              "stringr",
              "tools",
              "RMySQL",
              "stringr",
              "patchwork",
              "gt",
              "readxl",
              "readr",
              "stringr")
invisible(lapply(packages, library, character.only = TRUE))

load_dot_env()

fls <- list.files(path = 'data',pattern = "^tanken_[a-zA-Z]+\\.xlsx$")
df <- data.frame()
for(d in c('compagner','verheijen')){
  fl <- list.files(path  ='data',
                   full.names = T,
                   pattern = str_glue('^tanken_{d}\\.xlsx'))
  df <- rbind(df,read_xlsx(fl) %>% 
                select(c(1,4,5,6,8,13)) %>% 
                mutate(farm = d) %>% 
                rename(datum = 1,
                       kg = 2,
                       eiwit = 3,
                       vet = 4,
                       ureum  =5,
                       celgetal = 6)%>% 
                filter(!grepl('Gemiddelden|Totalen',datum)) %>% 
                mutate(datum = parse_date_time(datum,
                                               orders = "%d-%b-%Y",
                                               locale = "nl_NL.UTF-8"))
              )
}

ds <- read_xlsx('data/tanken_spiker.xlsx') %>% 
  select(1,3,5,6,8,12) %>% 
  rename(datum  =1,
         kg = 2,
         eiwit = 4,
         vet = 3,
         ureum  =5,
         celgetal =6) %>% 
  mutate(farm  = 'spiker',
         datum = parse_date_time(datum,orders = '%d-%m-%Y'))

dff <- rbind(df,ds) %>% 
  mutate(kg = as.numeric(gsub('\\.','',kg))) %>% 
  filter(kg > 5000) %>% 
  mutate(week = floor_date(datum,unit = 'week',week_start = 1)) |> 
  mutate(across(c(eiwit,vet,ureum,celgetal),
                \(x) as.numeric(gsub(",","\\.",x))))|> 
  pivot_longer(cols = !c(datum,farm,week)) |> 
  filter(!is.na(value))


## GRAFIEKEN EIWIT, UREUM ,...

for(VAR in c(
  'PG_HOST',
  'PG_DB',
  'PG_USER',
  'PG_PWD',
  'MS_HOST',
  'MS_DB',
  'MS_USER',
  'MS_PWD'
)){
  assign(VAR, Sys.getenv(VAR))
  if(get(VAR) == '') stop(paste0('Missing ', VAR))
}

if(!exists('pgdb') || !dbIsValid(pgdb)){
  pgdb <- dbConnect(Postgres(), host = PG_HOST, dbname = PG_DB, user = PG_USER, pass = PG_PWD)
  quer_pg <- function(...) dbGetQuery(pgdb, str_glue(paste0(...)))
  pgex <- function(...) dbExecute(pgdb, paste0(...))
}

if(!exists('msdb')){
  msdb <- dbConnect(MySQL(), host = MS_HOST, dbname = MS_DB, user = MS_USER, pass = MS_PWD)
  quer_ms <- function(...) dbGetQuery(msdb, str_glue(paste0(...)))
  msex <- function(...) dbExecute(msdb, paste0(...))
}

nms <- c(
         "verheijen",
         "compagner",
         'mtsspiker'
         )


sql <- glue::glue_sql("select id
                      from tbl_999_users
                      where user in ({nms*})",
                      .con = msdb)
kids <- quer_ms(sql)[,1]

mindat <- "2024-01-01"
maxdat <- "2026-08-15"

## data voerkosten
## mineff
sql <- glue::glue_sql('select user_id,date,intake_re,intake_p,frac_dm_forage from view_min_eff
                     where user_id in ({kids*})
                     and date between {mindat} and {maxdat}',
                     .con = msdb)
mineffs <- quer_ms(sql)

## voerkosten
sql <- glue::glue_sql(
  'select u.user,user_id,
  date,
  dim,
  production_kg,
  amount_conc_100_milk,
  fpcm,intake_kg_dm,
  fe,
  feed_cost,
  feed_cost_bought
  from view_production_cost 
  inner join tbl_999_users u on u.id = user_id
  where user_id in ({kids*})
                        and date between {mindat} and {maxdat}',
  .con = msdb
)

fe <- quer_ms(sql)
tabfe <- fe |> inner_join(mineffs,by = c('user_id','date')) |>
  mutate(user = gsub("mts","",user),
         date = as.Date(date)) |> 
  pivot_longer(cols = !c(date,user,user_id))

## productiedata

nms <- c(
  "Verheijen",
  "Compagner",
  'spiker'
)

sql <- glue::glue_sql("select name,id
                      from farms
                      where name in ({nms*})",
                      .con = pgdb)
farms <- quer_pg(sql) |> 
  rename(farm = name)

mindat <- '2026-01-01'

sql <- glue::glue_sql("select farms_id,reg_id,date_time,production,protein_pct,fat_pct,lactation,dim 
                      from view_productions
                      where farms_id in ({farms$id*})
                      and date_time between {mindat} and {maxdat}
                      union all
                      select farms_id,reg_id,date_time,production,protein_pct,fat_pct,lactation,dim 
                      from view_melkcontrole
                      where farms_id in ({farms$id*})
                      and date_time between {mindat} and {maxdat}",
                      .con = pgdb)

prdata <- quer_pg(sql) |> 
  group_by(farms_id,date_time,reg_id,lactation,dim) |> 
  summarize(production = sum(production,na.rm = T),
            protein_pct = mean(protein_pct,na.rm = T),
            fat_pct = mean(fat_pct,na.rm = T)) |> 
  pivot_longer(cols = !c(date_time,reg_id,farms_id,lactation,dim)) |> 
  inner_join(farms,by = c("farms_id" = "id")) |> 
  filter(value>0,
         !is.na(value)) |> 
  mutate(farm = tolower(farm)) |> 
  ungroup() |> 
  select(-reg_id)


save(dff,tabfe,prdata,file= "data/data.Rdata")


