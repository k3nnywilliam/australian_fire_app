
box::use(
  shiny[reactiveValues],
)

cat("Loading data...\n", file = stderr())
fire_nrt_V1_96617_df <- readr::read_csv('app/data/fire_nrt_V1_96617.csv') |>
  data.table::as.data.table()
fire_nrt_V1_96617_df <- reactiveValues(val = fire_nrt_V1_96617_df)

fire_archive_V1_96617_df <- readr::read_csv('app/data/fire_archive_V1_96617.csv')
fire_archive_V1_96617_df <- data.table::as.data.table(fire_archive_V1_96617_df)
fire_nrt_M6_96619_df <- readr::read_csv('app/data/fire_nrt_M6_96619.csv')
fire_nrt_M6_96619_df<- data.table::as.data.table(fire_nrt_M6_96619_df)
fire_archive_M6_96619_df <- readr::read_csv('app/data/fire_archive_M6_96619_mod.csv')
fire_archive_M6_96619_df <- data.table::as.data.table(fire_archive_M6_96619_df)
cat("Loading data done.\n", file = stderr())


