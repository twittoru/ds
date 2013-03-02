setRefClass("packer",
  methods = list(
    .pack_array = function(object,len) {
      if(len < 16) {
        prefix <- as.raw(0x90) | as.raw(len)
      } else if(len < 2^16) {
        prefix <- as.raw(c(0xdc,len %/% 2^8,len %% 2^8))
      } else {
        len_l <- len %% 2 ^ 16
        len_m <- len %/% 2 ^ 16
        prefix <- as.raw(c(
          0xdd,
          len_m %/% 2^8,
          len_m %% 2^8,
          len_l %/% 2^8,
          len_l %% 2^8
        ))
      }
      rest <- sapply(object,pack,USE.NAMES = F)
      return(c(prefix,unlist(rest)))
    },
    .pack_map = function(object,len,name) {
      if(len < 16) {
        prefix <- as.raw(0x80) | as.raw(len)
      } else if(len < 2^16) {
        prefix <- as.raw(c(0xde,len %/% 2^8,len %% 2^8))
      } else {
        len_l <- len %% 2 ^ 16
        len_m <- len %/% 2 ^ 16
        prefix <- as.raw(c(
          0xdf,
          len_m %/% 2^8,
          len_m %% 2^8,
          len_l %/% 2^8,
          len_l %% 2^8
        ))
      }
      rest <- sapply(1:len,function(n) {
        c(
          pack(name[n]),
          pack(getElement(object,n))
        )
      },USE.NAMES = F)
      return(c(prefix,unlist(rest)))
    },
    .pack_multi = function(object) {
      name <- names(object)
      len <- length(object)
      if(is.null(name)) {
        #Array
        .pack_array(object,len)
      } else {
        #Map
        .pack_map(object,len,name)
      }
    },
    .pack_numeric    = function(object) {
      if(is.outofint(object)) {
        .pack_double(object)
      } else if(object < 0) {
        .pack_integer(object)
      } else {
        .pack_unsignedinteger(object)
      }
    },
    .pack_double = function(object) {
      if(is.infinite(object)) {
        s <- sign(object) < 0   #sign
        e <- rep(1,11)          #exponent = 1
        f <- rep(0,52)          #fraction = 0
      } else if(is.nan(object)) {
        #NaN: R cannot distinguish NaNs between quiet NaN and signaling NaN.
        s <- 1                  #sign
        e <- rep(1,11)          #exponent = 1
        f <- c(1,               #fraction = +type of NaN
          rep(0,51)             #           |extra payload
        )
      } else  {
        bits <- doubleToBits(object)
        s <- bits[1]
        e <- bits[2:12]
        f <- bits[13:64]
      }
      return(c(as.raw(0xcb),rev(packBits(as.integer(rev(c(
        s,             #sign
        e,             #exponent
        f              #fraction
      )))))))
    },
    .pack_fixnum = function(object,sign) {
      return(rev(packBits(rev(
        `if`(sign,c(T,T,T,intToBits(object)[5:1]),c(F,intToBits(object)[7:1]))
      ))))
    },
    .pack_unsignedinteger = function(object) {
      if(object >= 0 && object < 128) return(.pack_fixnum(object,0))
      # R can not represent UINT32, UINT64
      size_T <- c(rep(0xcc,8),rep(0xcd,8),rep(0,16))
      size <- rev(size_T[as.logical(intToBits(object))])[1]
      if(!size){
        message(sprintf("uint32 value %d is recognized as int32",object))
        return(.pack_integer(object))
      }
      binary <- rev(packBits(intToBits(object)[1:(2^(size-0xcc+3))]))
      return(c(as.raw(size),binary))
    },
    .pack_integer = function(object) {
      if(object < 0 && object > -33) return(.pack_fixnum(object,1))
      # R can not represent INT64
      size_T <- c(rep(0xd0,8),rep(0xd1,8),rep(0xd2,16))
      size <- rev(size_T[as.logical(intToBits(abs(object) ))])[1]
      binary <- rev(packBits(intToBits(object)[1:(2^(size-0xd0+3))]))
      return(c(as.raw(size),binary))
    },
    .pack_character  = function(object) {
      .pack_raw(charToRaw(object))
    },
    .pack_raw        = function(object) {
      len <- length(object)
      if(len < 31) {
        #FixRaw
        return(c(as.raw(0xa0) | as.raw(len), object))
      } else if(len < 2^16) {
        #Raw16
        return(c(as.raw(0xda),rev(packBits(intToBits(len)[1:16])),object))
      } else {
        #Raw32
        len_l <- len %% 2 ^ 16
        len_m <- len %/% 2 ^ 16
        prefix <- as.raw(c(
          0xda,
          len_m %/% 2^8,
          len_m %% 2^8,
          len_l %/% 2^8,
          len_l %% 2^8
        ))
        return(c(prefix,object))
      }
    },
    .pack_logical    = function(object) {
      if(is.na(object)) {
        #NA
        return(as.raw(0xc0))
      } else if(object) {
        #TURE
        return(as.raw(0xc3))
      } else {
        #FALSE
        return(as.raw(0xc2))
      }
    },
    pack = function(object) {
      len <- length(object)
      value <- NULL
      if(len > 1 || is.list(object)) {
        value <- .pack_multi(object)
      } else {
        klass <- class(object)
        if(!is.nan(object) && is.na(object)) {
          value <- .pack_logical(object)
        } else {
          value <- switch(klass,
            numeric = .pack_numeric(object),
            character = .pack_character(object),
            logical = .pack_logical(object),
            raw = .pack_raw(object),
            integer = .pack_numeric(object)
          )
        }
      }
      if(is.null(value)) stop(sprintf("class %s (value %s) is unsupported",klass,value))
      return(value)
    }
  )
)
