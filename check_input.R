# [DESCRIPTION]
# check_input function checks 
# whether data is of certain class, length;
# whether elements of data are within a set;
# whether unexpected NAs present in data

# [INPUTS]
# data (integer/numeric/logical/character/factor): data to be checked (see note 1
#                                                  for accepted atomic data class)
# nme (character/null): the reference to data in the error msg
# cls (character/null): expected data class (if null, data class is not tested)
#                       (see note 1 for more details)
#                       Note that cls test is carried out regardless of data length
# len (integer/null): expected data length (if null, data length is not tested)
# min_len (integer/null): min data length (if null, min data length is not tested)
#                         (if len is not null, min data length is not tested)
# max_len (integer/null): max data length (if null, max data length is not tested)
#                         (if len is not null, max data length is not tested)
# val (same or comparable atomic class as data /null):
#                       acceptable elements in data
#                       (NA should not %in% val, use allow_na if need to include NA)
#                       If cls is specified, atomic class of val is tested against cls
#                       (see note 2 for more details).
#                       If cls is not specified, atomic class of val is tested against
#                       the atomic class of data (data_cls in the code).
#                       The only exception: character can be used when cls is
#                       'factor' or cls is NULL but data is factor.
#                       Note that val test is performed only when data length > 0
# allow_na (logical): allow NA in data or not
#                     Note that allow_na test is performed only when data length > 0

# [OUTPUTS]
# If check failed, an error will be raised. Otherwise, no output.

# [NOTES]
# 1. The current implementation only support following classes
#    [atomic class] integer: is.integer is TRUE
#    [atomic class] numeric: is.numeric is TRUE but is.integer is FALSE
#    [atomic class] logical: is.logical is TRUE
#    [atomic class] character: is.character is TRUE
#    [atomic class] factor: is.factor is TRUE
#    [aggregated class] num: union of integer and numeric atomic classes
#    [aggregated class] char: union of character and factor atomic classes
# 2. When cls is set to num or char, we are more cared about the
#    information rather than the format of the information. For example,
#    when we want to calculate the square root of a positive number,
#    sqrt(5L) and sqrt(5.0) should produce the same result (in mathematical
#    calculation, 5L and 5.0 mean the same thing). As a natural consequence
#    of this, if val is not NULL, its class will be checked against cls
#    rather than the atomic class of data. To demonstrate the feasibility
#    of using val of different atomic class as data
#    5L %in% c(1.0, 5.0) # returns TRUE
#    5.0 %in% c(1L, 5L) # returns TRUE
#    'a' %in% factor(c('a', 'b')) # returns TRUE
#    factor('a') %in% c('a', 'b') # returns TRUE

check_input <- function(data, nme=NULL,
                        cls=NULL,
                        len=NULL, min_len=NULL, max_len=NULL,
                        val=NULL, allow_na=FALSE) {
  # check whether data is of supported class
  cls_list <- c('integer', 'numeric', 'logical', 'character', 'factor')
  idx <- c(is.integer(data), is.numeric(data) & !is.integer(data),
           is.logical(data), is.character(data), is.factor(data))
  if( sum(idx) != 1 ) {
    stop("The class of data is not supported")
  }
  data_cls <- cls_list[idx]

  # check input nme
  if( !is.null(nme) ) {
    nme <- as.character(nme)
    if( length(nme) != 1 ) {
      stop("Length of nme should be 1")
    }
    if( is.na(nme) | grepl("^[[:space:]]*$", nme) ) {
      nme <- 'data'
    }
  } else {
    nme <- 'data'
  }

  # ------------------------- check class ---------------------------
  if( !is.null(cls) ) {
    cls_list_ext <- c(cls_list, 'num', 'char')

    # check input cls
    if( !(is.character(cls) & length(cls) == 1) ) {
      stop("cls should be a string")
    }
    if( !(cls %in% cls_list_ext) ) {
      stop("Class specified by cls is not supported")
    }

    # check data
    if( cls == 'num' ) {
      if( !(data_cls %in% c('numeric', 'integer')) ) {
        stop(paste0(nme, " should be numeric"))
      }
    } else if( cls == 'char' ) {
      if( !(data_cls %in% c('character', 'factor')) ) {
        stop(paste0(nme, " should be character"))
      }
    } else {
      if( cls != data_cls ) {
        stop(paste0(nme, " should be ", cls))
      }
    }
  } else {
    cls <- data_cls
  }

  # ------------------------ check length ----------------------------
  # check input len, min_len, max_len
  len_param <- list(len=len, min_len=min_len, max_len=max_len)
  for( k in 1:3 ) {
    if( k == 1 | (k > 1 & is.null(len_param$len)) ) {
      if( !is.null(len_param[[k]]) ) {
        if( !(is.integer(len_param[[k]]) & length(len_param[[k]]) == 1) ) {
          stop(paste0(names(len_param)[k], " should be an integer of length 1"))
        }
        if( is.na(len_param[[k]]) ) {
          stop(paste0(names(len_param)[k], " should not be NA, use NULL instead"))
        } else if( len_param[[k]] < 1 ) {
          stop(paste0(names(len_param)[k], " should be positive"))
        }
      }
    }
  }

  # check data
  if( !is.null(len) ) {
    if( length(data) != len ) {
      stop(paste0("Length of ", nme, " should be ", len))
    }
  } else if( is.null(min_len) & !is.null(max_len) ) {
    if( length(data) > max_len ) {
      stop(paste0("Maximum length of ", nme, " should be ", max_len))
    }
  } else if( !is.null(min_len) & is.null(max_len) ) {
    if( length(data) < min_len ) {
      stop(paste0("Minimum length of ", nme, " should be ", min_len))
    }
  } else if( !is.null(min_len) & !is.null(max_len) ) {
    if( min_len > max_len ) {
      stop("min_len should be no greater than max_len")
    }
    if( length(data) < min_len | length(data) > max_len ) {
      stop(paste0("Minimum/maximum length of ", nme, " should be ",
                  min_len, "/", max_len))
    }
  }

  # -------------------------- check na ------------------------------
  # check input allow_na
  if( !(is.logical(allow_na) & length(allow_na) == 1) ) {
    stop("allow_na should be either TRUE or FALSE")
  }
  if( !(allow_na %in% c(TRUE, FALSE)) ) {
    stop("allow_na should be either TRUE or FALSE")
  }

  # check data
  if( !allow_na ) {
    # if data has 0 length, any(is.na(data)) is FALSE
    if( any(is.na(data)) ) {
      stop(paste0("NA found in ", nme))
    }
  }

  # -------------------------- check val -----------------------------
  # check input val
  if( !is.null(val) ) {
    if( switch(cls, 'integer'=!is.integer(val),
                    'numeric'=!(is.numeric(val) & !is.integer(val)),
                    'logical'=!is.logical(val),
                    'character'=!is.character(val),
                    'factor'=!(is.factor(val) | is.character(val)),
                    'num'=!((is.numeric(val) & !is.integer(val)) |
                            is.integer(val)),
                    'char'=!(is.factor(val) | is.character(val)),
                    TRUE) ) {
      stop(paste0("val should be ", cls))
    }
    if( length(val) == 0 ) {
      stop("Length of val should not be 0, use NULL instead")
    } else if( any(is.na(val)) ) {
      stop("NA should not present in val, use allow_na instead")
    }
  }

  # check data
  if( !is.null(val) ) {
    val <- unique(val)
    if( length(data) > 0 ) {
      data_na_rm <- data[!is.na(data)]
      if( length(data_na_rm) > 0 ) {
        if( !all(data_na_rm %in% val) ) {
          selected_val <- val[1:min(length(val), 4)]
          stop(paste0(nme, " contains unexpected elements. ",
                      "Acceptable elements are: ",
                      paste(selected_val, collapse=", "),
                      ifelse(length(val) > 4, '...', '')))
        }
      }
    }
  }
}