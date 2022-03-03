#' # Get data from a local NASIS Database
#' 
#' This function loads tables from local NASIS Database. Its basic elements are
#' modified from 
#' 
#'  - get_mapunit_from_NASIS(), and
#'  - get_component_data_from_NASIS_db.R list of functions.
#'  
#'  In addition it depends on `R/sysdata.rda` which is a list item containing
#'  all queries and the `metadata` 
#'  table (see `\misc\SQL-scripts\convert-sql-to-rda.R`).
#'  

#' @param SS fetch data from the currently loaded selected set in NASIS or 
#' from the entire local database (default: `TRUE`)
#' @param dsn Optional: path to local SQLite database containing NASIS
#' #' table structure; default: `NULL`
#' @param table.name The physical name of NASIS table to load. Options 
#' are: "mapunit", "component", "chorizon"
#' @param format if `FALSE` the queried table is returned as is. 
#' Default is `TRUE` and runs table formatting operations from individual
#' `get_*` functions.
#' @param custom_SQL if custom SQL script is supplied, `table.name` is ignored
#' and instead a user provided custom SQL scrip is run to query from NASIS. 
#' Default is set to `FALSE`.
#' @param SQL_script if `custom_SQL=TRUE`, an SQL script to be run against
#' local NASIS Database
#' @export
get_data_from_NASIS <- function(SS = TRUE, dsn = NULL, 
                                table.name, 
                                custom_SQL = FALSE, 
                                SQL_script = NULL, 
                                format = TRUE, 
                                # variables required if format == TRUE
                                stringsAsFactors = default.stringsAsFactors(), # formatting
                                droplevels = TRUE, # mapunit formatting
                                nullFragsAreZero = TRUE, #component and horizon formatting
                                fill = FALSE # horizon formatting
                                ) {
  if(custom_SQL){
    q = SQL_script
  }else{
    table.names.choice <- c("mapunit", "component", "chorizon")
    if(!table.name %in% table.names.choice & !custom_SQL){
      table.name.text = paste0(table.names.choice, collapse = ", ")
      stop(paste("`table.name` must be one of", 
                 table.name.text))
    }
    
    q = soilDB:::qmeta_lst[[table.name]]
  }
  
  
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }
  
  channel <- dbConnectNASIS(dsn)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())
  
  # exec query
  d <- dbQueryNASIS(channel, q)
  
  # apply table formatting 
  if(!custom_SQL &
     format) {
    d <- format_table_NASIS(
      d = d,
      table.name = table.name,
      SS = SS,
      dsn = dsn,
      stringsAsFactors = stringsAsFactors, # formatting
      droplevels = droplevels, # mapunit formatting
      nullFragsAreZero = nullFragsAreZero, #component and horizon formatting
      fill = fill 
    )
  }
  
  # done
  return(d)
}





format_table_NASIS <- function(d, table.name,
                               SS = TRUE,
                               droplevels = TRUE, 
                               stringsAsFactors = default.stringsAsFactors(), 
                               nullFragsAreZero = TRUE, #component and horizon formatting
                               fill = FALSE, # horizon formatting
                               dsn = NULL) {
  
  if(table.name == "mapunit"){
    # recode metadata domains
    fd <- uncode(
      d,
      db = "NASIS",
      droplevels = droplevels,
      stringsAsFactors = stringsAsFactors,
      dsn = dsn
    )
    
    # hacks to make R CMD check --as-cran happy:
    metadata <- NULL
    
    # load local copy of metadata
    metadata = soilDB:::qmeta_lst$metadata
    
    # transform variables and metadata
    fd <- within(fd, {
      farmlndcl = factor(farmlndcl,
                         levels = metadata[metadata$ColumnPhysicalName == "farmlndcl", "ChoiceValue"],
                         labels = metadata[metadata$ColumnPhysicalName == "farmlndcl", "ChoiceLabel"]
      )
      if (stringsAsFactors == FALSE) {
        farmlndcl = as.character(farmlndcl)
      }
      if (droplevels == TRUE & is.factor(farmlndcl)) {
        farmlndcl = droplevels(farmlndcl)
      }
    })
    
  }else if(table.name=="component"){
    # test for duplicate coiids
    idx <- which(table(d$coiid) > 1)
    if (length(idx) > 0) {
      dupes <- names(idx)
      assign('dupe.coiids', value=dupes, envir=soilDB.env)
      message("-> QC: duplicate coiids, this should not happen.\n\tUse `get('dupe.coiids', envir=soilDB.env)` for component record IDs (coiid)")
    }
    q2 <- "SELECT * FROM cosurffrags_View_1"
    # toggle selected set vs. local DB
    if (SS == FALSE) {
      q2 <- gsub(pattern = '_View_1', replacement = '', x = q2, fixed = TRUE)
    }
    channel <- dbConnectNASIS(dsn)
    # surface fragments
    chs <- simplifyFragmentData(
      uncode(dbQueryNASIS(channel, q2), dsn = dsn),
      id.var = "coiidref",
      vol.var = "sfragcov_r",
      prefix = "sfrag",
      msg = "surface fragment cover")
    
    colnames(chs) <- paste0("surface_", colnames(chs))
    colnames(chs)[1] <- "coiidref"
    
    # uncode metadata domains
    if (nrow(d) > 0) {
      d <- uncode(d, stringsAsFactors = stringsAsFactors, dsn = dsn)
      
      ldx <- !d$coiid %in% chs$coiidref
      if (!any(ldx)) {
        chs <- chs[1:nrow(d),]
        chs$coiidref <- d$coiid
      } else {
        chs_null <- chs[0, ][1:sum(ldx), ]
        chs_null$coiidref <- d$coiid[ldx]
        chs <- rbind(chs, chs_null)
      }
      
      # handle NA for totals
      if (nullFragsAreZero) {
        chs[is.na(chs)] <- 0
      } 
      fd <- merge(d, chs, by.x = "coiid", by.y = "coiidref", all.x = TRUE, sort = FALSE)
    } else {
      fd <- cbind(d, chs[0,])
    }
    
    
  }else if(table.name=="chorizon"){
    q2 <- "SELECT * FROM chfrags_View_1"
    q3 <- "SELECT * FROM chhuarts_View_1"
    
    # toggle selected set vs. local DB
    if (SS == FALSE) {
      q2 <- gsub(pattern = '_View_1', replacement = '', x = q2, fixed = TRUE)
      q3 <- gsub(pattern = '_View_1', replacement = '', x = q3, fixed = TRUE)
    }
    if (fill == FALSE) {
      d <- d[!is.na(d$chiid), ]
    }
    channel <- dbConnectNASIS(dsn)
    # "sieving" chfrags, chuarts tables for parity with fetchNASIS("pedons") @horizons slot columns
    
    if (nrow(d) > 0){
      # horizon fragments
      chf <- simplifyFragmentData(
        uncode(dbQueryNASIS(channel, q2, close = FALSE), dsn = dsn),
        id.var = "chiidref",
        vol.var = "fragvol_r",
        nullFragsAreZero = nullFragsAreZero
      )
      if (sum(complete.cases(chf)) == 0) {
        chf <- chf[1:nrow(d),]
        chf$chiidref <- d$chiid
      } else {
        ldx <- !d$chiid %in% chf$chiidref
        chf_null <- chf[0,]
        if (any(ldx)) {
          chf_null <- chf_null[seq(sum(ldx)),]
        }
        chf_null$chiidref <- d$chiid[ldx]
        chf <- rbind(chf, chf_null)
      }
      # handle NA for totals
      if (nullFragsAreZero) {
        chf[is.na(chf)] <- 0
      }
      
      # human artifacts
      cha <- simplifyArtifactData(
        uncode(dbQueryNASIS(channel, q3, close = FALSE), dsn = dsn),
        id.var = "chiidref",
        vol.var = "huartvol_r",
        nullFragsAreZero = nullFragsAreZero
      )
      # handle NULL result
      if (sum(complete.cases(cha)) == 0) {
        cha <- cha[1:nrow(d),]
        cha$chiidref <- d$chiid
      } else {
        ldx <- !d$chiid %in% cha$chiidref
        cha_null <- cha[0,][1:sum(ldx),]
        cha_null$chiidref <- d$chiid[ldx]
        cha <- rbind(cha, cha_null)
      }
      # handle NA for totals
      if (nullFragsAreZero) {
        cha[is.na(cha)] <- 0
      }
      
      
      d <- merge(d, chf, by.x = "chiid", by.y = "chiidref", all.x = TRUE, sort = FALSE)
      fd <- merge(d, cha, by.x = "chiid", by.y = "chiidref", all.x = TRUE, sort = FALSE)
    }
  }
  
  
  # done
  return(fd)
}

