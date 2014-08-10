# [DESCRIPTION]
# check_input function checks 
# whether data is of certain class, length;
# whether elements of data are within a set;
# whether unexpected NAs present in data

# [INPUTS]
# data (integer/numeric/logical/character/factor):
#                       data to be checked (see notes for more details)
# nme (character/null): the reference to data in the error msg
# cls (character/null): expected data class (if null, data class is not tested)
#                       (see notes for more details)
#                       *** cls test is carried out regardless of data length ***
# len (integer/null): expected data length (if null, data length is not tested)
# min_len (integer/null): min data length (if null, min data length is not tested)
# max_len (integer/null): max data length (if null, max data length is not tested)
# val (same class as data/null): acceptable elements in data
#                                character can be used when data is factor
#                                integer can be used when data is numeric
#                                *** val test is performed only when data length > 0 ***
# allow_na (logical): allow NA in data or not
#                     *** allow_na test is performed only when data length > 0 ***

# [OUTPUTS]
# If check failed, an error will be raised. Otherwise, no output.

# [NOTES]
# The current implementation only support following classes
# integer, numeric, logical, character, factor

check_input <- function(data, nme=NULL,
                        cls=NULL,
                        len=NULL, min_len=NULL, max_len=NULL,
                        val=NULL, allow_na=FALSE) {
  # check whether data is of supported class
  # null class at the moment is not supported
  cls_list <- c('integer', 'numeric', 'logical', 'character', 'factor')
  idx <- c(is.integer(data), is.numeric(data), is.logical(data),
           is.character(data), is.factor(data))
  if( sum(idx) != 1 ) {
    if( sum(idx) == 2 & is.integer(data) & is.numeric(data) &
        identical(class(data), 'integer') ) {
      data_cls <- 'integer'
    } else {
      stop("check_input() only supports int/num/logi/char/factor")
    }
  } else {
    data_cls <- cls_list[idx]
  }

  # check input nme
  if( !is.null(nme) ) {
    if( !(is.character(nme) & length(nme) == 1) ) {
      stop("nme should be a string")
    }
    if( is.na(nme) | grepl("^[[:space:]]*$", nme) ) {
      nme <- 'data'
    }
  } else {
    nme <- 'data'
  }

  # ------------------------- check class ---------------------------
  if( !is.null(cls) ) {
    # check input cls
    if( !(is.character(cls) & length(cls) == 1) ) {
      stop("cls should be a string")
    }
    if( !(cls %in% cls_list) ) {
      stop("check_input() only supports int/num/logi/char/factor")
    }
    # check data
    if( cls != data_cls ) {
      stop(paste0("Class of ", nme, " should be ", cls))
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
          len_param[[k]] <- NULL
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
      stop(paste0("Minimum/maximum length of ", nme, " should be ", min_len, "/", max_len))
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
    # if data has 0 length, sum(is.na(data)) is equal to 0
    if( sum(is.na(data)) != 0 ) {
      stop(paste0("NA found in ", nme))
    }
  }

  # -------------------------- check val -----------------------------
  # check input val
  if( !is.null(val) ) {
    if( switch(cls, 'integer'=!is.integer(val),
                    'numeric'=!is.numeric(val),
                    'logical'=!is.logical(val),
                    'character'=!is.character(val),
                    'factor'=!(is.factor(val) | is.character(val))) ) {
      stop(paste0("Class of val should be ", cls))
    }
    if( length(val) == 0 ) {
      val <- NULL
    } else if( sum(is.na(val)) != 0 ) {
      stop("NA should not present in val, use allow_na instead")
    }
  }
  # check data
  if( !is.null(val) ) {
    if( length(data) > 0 ) {
      val <- unique(val)
      if( allow_na ) {
        val <- c(val, NA)
      }
      if( sum(data %in% val) != length(data) ) {
        selected_val <- val[1:min(length(val), 4)]
        stop(paste0(nme, " contains unexpected elements. ",
                    "Acceptable elements are: ",
                    paste(selected_val, collapse=", "),
                    ifelse(length(val) > 4, '...', '')))
      }
    }
  }
}