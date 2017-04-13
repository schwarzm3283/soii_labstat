pw <- {
  "pw"
}

pool <- dbPool(
  drv = dbDriver("PostgreSQL"),
  dbname = "dbname",
  host = "localhost",
  port = 5432,
  user = "user",
  password = pw
)

rm(pw)