#' # Convert SQL and metadata to `sysdata.rda`
#' 
#' Script to create `R/sysdata.rda` file which is loaded by default when
#' package is installed (provided that Lazy loading is set to TRUE in 
#' the package `DESCRIPTION` file).
#' 
#' The contents of this data are:
#' 
#' (1) SQL queries converted from `.sql` files into
#' items on a list `sysdata.rda`.
#' 
#' (2) The `data/metadata.rda` table as a list entry.
#' 
#' 
#' ## (1) SQL scripts
#' the `.sql` files are read from within the 'misc/SQL-scripts' folder. 

# list files ending with ".sql" in folder
filenames_lst = list.files(path = file.path("misc", "SQL-scripts"), 
                           pattern = ".sql$")

read_lines_fun = function(filename){
  sql_i = readLines(con = file.path("misc", "SQL-scripts",filename), 
                    warn = FALSE)
  sql_i = paste(sql_i, collapse = "\n")
  return(sql_i)
}
# named list of sql statements
q_lst = lapply(filenames_lst, read_lines_fun)
# add names to list items based on filename.
names(q_lst) = sub("\\..*", "", filenames_lst)

#' ## (2) Metadata table
load("data/metadata.rda")
meta_lst = list(metadata = metadata)
qmeta_lst = append(q_lst, meta_lst)


usethis::use_data(qmeta_lst, internal = T, overwrite = T)
