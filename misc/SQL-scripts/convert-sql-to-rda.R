#' ## Script to read `.sql` files and convert them into `sysdata.rda`
#' 
#' This is saved under `R/sysdata.rda` and is 
#' accessible for functions inside the package. Lazy loading is set to TRUE in
#' the `DESCRIPTION`.

# list files ending with ".sql" in folder
filenames_lst = list.files(path = file.path("misc", "SQL-scripts"), 
                           pattern = ".sql$")

read_lines_fun = function(filename){
  sql_i = readLines(con = file.path("misc", "SQL-scripts",filename), 
                    warn = FALSE)
  sql_i = paste(sql_i, collapse = " ")
  #names(sql_i) = sub("\\..*", "", filename)
  return(sql_i)
}
# named list of sql statements
q.lst = lapply(filenames_lst, read_lines_fun)
names(q.lst) = sub("\\..*", "", filenames_lst)

usethis::use_data(q.lst, internal = T, overwrite = T)
