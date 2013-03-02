setRefClass("packer",
  methods = list(
    .pack_array = function(object) {
      in_array <- paste(sapply(object,pack),collapse=",")
      paste("[",in_array,"]",sep="")
    },
    .pack_map = function(object,name) {
      if("" %in% name) stop()
      in_object <- paste(sapply(name,.pack_character),sapply(object,pack),sep=":",collapse=",")
      paste("{",in_object,"}",sep="")
    },
    .pack_multi = function(object) {
      name <- names(object)
      len <- length(object)
      if(is.null(name)) {
        #Array
        .pack_array(object)
      } else {
        #Map
        .pack_map(object,name)
      }
    },
    .pack_numeric    = function(object) {
      if(is.nan(object) || is.infinite(object)){ stop("unsupported") }
      sprintf("%g",object)
    },
    .pack_character  = function(object) {
      encodeString(object, quote='"')
    },
    .pack_raw        = function(object) {
      .pack_character(rawToChar(object))
    },
    .pack_null = function(object) {
      if(is.nan(object)) stop("NaN is unsupported")
      "null"
    },
    .pack_logical    = function(object) {
      if(object) {
        "true"
      } else {
        "false"
      }
    },
    pack = function(object) {
      len <- length(object)
      value <- NULL
      if(len == 0) {
        if(is.list(object)) {
          value <- "{}"
        } else {
          value <- "[]"
        }
      } else if(len > 1) {
        value <- .pack_multi(object)
      } else {
        if(is.na(object)) return(.pack_null(object))
        klass <- class(object)
        value <- switch(klass,
          numeric = .pack_numeric(object),
          character = .pack_character(object),
          logical = .pack_logical(object),
          raw = .pack_raw(object),
          integer = .pack_numeric(object),
          list = .pack_multi(object)
        )
      }
      if(is.null(value)) stop(sprintf("class %s (value %s) is unsupported",klass,value))
      return(value)
    }
  )
)
