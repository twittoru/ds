setRefClass("unpacker",
  fields = c(
    "stream"
  ),
  methods = list(
    initialize = function() {
    },
    .skip = function() {
      curr <- readChar(stream, 1)
      while(curr %in% c(" ","\n")) {
        curr <- readChar(stream, 1)
      }
      return(curr)
    },
    .readBack = function() {
      now <- seek(stream)
      seek(stream, now - 1)
    },
    unpack = function(json, remainder=F, is.inner=F) {
      if(!remainder) {
        flag <- tryCatch(!isOpen(stream),error = function(e) NULL)
        if (is.null(flag) || flag) {
          stream <<- rawConnection(charToRaw(json))
          on.exit({close(stream)})
        }
      }

      value <- NULL
      currentChar <- .skip()
      if (currentChar == "n") {
        # nil
        value <- NA
        readChar(stream, 3)
      } else if (currentChar == "f") {
        # false
        value <- FALSE
        readChar(stream, 4)
      } else if (currentChar == "t") {
        # true
        value <- TRUE
        readChar(stream, 3)
      } else if (currentChar %in% c("-",as.character(0:9))) {
        # number
        value <- .makeNumber(currentChar)
      } else if (currentChar == "\"") {
        # string
        value <- .makeString()
      } else if (currentChar == "[") {
        # array
        value <- .makeArray(is.inner)
      } else if (currentChar == "{") {
        # map
        value <- .makeMap()
      } else if (currentChar %in% c("}",":","]")) {
        .readBack()
      }
      if(!remainder && isIncomplete(stream)) warning("extra bytes exists")
      return(value)
    },
    .getKey = function() {
      key <- unpack(remainder=T)
      if(!is.character(key)) {
        stop(sprintf("invalid key: %s",key))
      }
      return(key)
    },
    .checkTypes = function(values) {
      if(all(sapply(values,length) == 1)) {
        types <- unique(unlist(lapply(values, deepclass)))
        len <- length(types)
        if (len == 1 && types != "list") {
          class(values) <- types
        }
      }
      return(values)
    },
    .makeInt = function(curr=NA,fraction=F) {
      sign <- ""
      if(is.na(curr)) {
        curr <- readChar(stream, 1)
      } else {
        curr <- curr
      }
      if(curr %in% c("-","+")) {
        sign <- curr
        if(fraction) stop
        curr <- readChar(stream, 1)
      }
      string <- rawConnection(raw(),open="r+")
      while(T) {
          if(length(curr) == 0) break
          if(curr %in% 0:9){
            writeChar(curr,string,eos=NULL)
          } else {
.readBack()
            break
          }
          curr <- readChar(stream, 1)
      }
      writeChar("\n",string,eos=NULL)
      flush(string)
      seek(string, 0)
      ret <- paste(sign,readLines(string),sep="")
      close(string)
      return(ret)
    },
    .makeNumber = function(curr) {
      sign <- ""
      if(curr == "-") {
        sign <- curr
      } else {
        .readBack()
      }
      int <- decimal <- exponent <- 0
      while(T) {
          curr <- readChar(stream, 1)
          if(length(curr) == 0) break
          if(curr == ".") {
            decimal <- .makeInt(fraction=T)
          } else if(curr %in% c("e","E")) {
            exponent <- .makeInt()
          } else if(curr %in% 0:9){
            int <- .makeInt(curr=curr)
          } else {
.readBack()
            break
          }
      }
      ret <- as.numeric(paste(sign,int,".",decimal,"E",exponent,sep=""))
      flag <- tryCatch(abs(ret - as.integer(ret)) < .Machine$double.eps, warning = function(e) F)
      if(flag) ret <- as.integer(ret)
      return(ret)
    },
    .decodeString = function() {
      utf16 <- readChar(stream, 4)
      n <- as.integer(paste(c("0x",utf16),collapse=""))
      intToUtf8(n)
    },
    .makeString = function() {
      string <- rawConnection(raw(),open="r+")
      escaped <- F
      while(T) {
        if(escaped) {
          control <- readChar(stream, 1)
          curr <- switch(control,
            "\"" = "\"",
            "\\" = "\\",
            "/" = "/",
            "b" = "\b",
            "f" = "\f",
            "n" = "\n",
            "r" = "\r",
            "t" = "\t",
            "u" = .decodeString()
          )
          writeChar(curr,string,eos=NULL)
          escaped <- F
        } else {
          curr <- readChar(stream, 1)
          escaped <- curr == "\\"
          if(escaped) next
          if(curr == "\"") break
          writeChar(curr,string,eos=NULL)
        }

      }
      writeChar("\n",string,eos=NULL)
      flush(string)
      seek(string, 0)
      ret <- paste(readLines(string), collapse="\n")
      close(string)
      return(ret)
    },
    .makeArray = function(is.inner) {
      arr <- vector(mode = "list")
      i <- 1
      while(T) {
        arr[[i]] <- unpack(remainder=T,is.inner=T)
        curr <- .skip()
        if(curr == "]") break
        if(curr != ",") stop("invalid array")
        i <- i + 1
      }
      if(length(arr) == 0) return(vector())
      if(is.inner) return(arr)
      arr <- .checkTypes(arr)
      return(arr)
    },
    .makeMap = function() {
      map <- vector(mode = "list")
      keys <- character()
      i <- 1
      while (T) {
        key <- unpack(remainder=T)
        if(is.null(key)) break
        keys[i] <- key
        curr <- .skip()
        if(curr == "}") break
        map[[i]] <- unpack(remainder=T)
        curr <- .skip()
        if(curr == "}") break
        if(curr != ",") stop(sprintf("%s,invalid object",curr))
        i <- i + 1
      }
      if(length(arr) == 0) return(list())
      names(map) <- keys
      map <- .checkTypes(map)
      return(map)
    }
  )
)
