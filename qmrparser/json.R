library(qmrparser)

second <- function(...) concatenation(..., action=function(s) s[[2]])
first  <- function(...) concatenation(..., action=function(s) s[[1]])
alt    <- function(...) alternation(..., action=I)
opt    <- function(...) option(..., action=I)
star   <- function(...) option(repetition1N(..., action=I), action=emptyToNull)
nth    <- function(n,typename="",...) concatenation(..., action=function(s) list(type=typename,value=s[[n]]))

parsePaste <- function(typename="", postfunction=I) {
  function(s) {
    list(
      type=typename,
      value=postfunction(paste(sapply(s, function(ss) ss$value), collapse=""))
    )
  }
}

emptyToNull <- function(s) {
  if(!is.null(s$type) && s$type == "empty") {
    NULL
  } else {
    s
  }
}

digistParse <- function(s) {
  if(s$type == "empty") {
    s
  } else {
    parsePaste("digit")(s$value)
  }
}

listAppend <- function(s) {
  if(length(s) == 2) {
    append(s[1], s[[2]])
  } else {
    s
  }
}

element <- function(a) opt(concatenation(a, star(second(comma, a)), action=listAppend))

json_value <- function() alt(
  json_string, json_number, json_object, json_array, json_true, json_false, json_null
)

json_number <- {
  minus <- alt(charParser("-"), empty())
  signflag <- alt(charParser("-"), charParser("+"), empty())
  zero <- charParser("0", action = function(s) list(type="number", value=0))
  dot <- charParser(".")
  expflag <- charInSetParser(function(ch) tolower(ch) == "e")
  one2nine <- charInSetParser(function (ch) "1" <= ch && ch <= "9")
  digits <- repetition0N(charInSetParser(isDigit), action=digistParse)

  intpart <- alt(zero, concatenation(one2nine, digits, action=parsePaste("digit")))
  decimal <- concatenation(dot, numberNatural(), action=parsePaste("digit"))
  exponent <- concatenation(expflag, signflag, numberNatural(), action=parsePaste("digit"))

  concatenation(minus, intpart, opt(decimal), opt(exponent), action=parsePaste("number", as.numeric))
}

json_string <- string(action=function(s) list(type="string", value=s))
json_true  <- keyword("true",  action=function(s) list(type="true",  value=T))
json_false <- keyword("false", action=function(s) list(type="false", value=F))
json_null  <- keyword("null",  action=function(s) list(type="null",  value=NA))
json_array <- {
  withWhiteSpace <- function(a) second(whitespace(), first(a, whitespace()))
  array_start <- withWhiteSpace(charParser("["))
  array_end <- withWhiteSpace(charParser("]"))
  comma <- withWhiteSpace(charParser(","))
  nth(2,typename="array",array_start, element(json_value()), array_end)
}
json_object <- {
  withWhiteSpace <- function(a) second(whitespace(), first(a, whitespace()))
  object_start <- withWhiteSpace(charParser("{"))
  object_end   <- withWhiteSpace(charParser("}"))
  colon        <- withWhiteSpace(charParser(":"))
  comma <- withWhiteSpace(charParser(","))
  object_element <- concatenation(json_string, colon, json_value(), action=function(s) list(name=s[[1]]$value, value=s[[3]]))
  nth(2,typename="object",object_start, element(object_element), object_end)
}

deepclass <- function(x) {
  if(length(unlist(x)) > 1) return(lapply(x,deepclass))
  ifelse(is.na(x),list(),class(x))
}
checkTypes <- function(values) {
  if(all(sapply(values,length) == 1)) {
    types <- unique(unlist(lapply(values, deepclass)))
    len <- length(types)
    if (len == 1 && types != "list") {
      class(values) <- types
    }
  }
  return(values)
}
trans_object <- function(node) {
  if(!is.null(node$value$type) && node$value$type == "empty") {
    list()
  } else {
    name <- lapply(node$value,function(n) n$name)
    values <- lapply(node$value,function(n) translate(n$value,T))
    names(values) <- name
    checkTypes(values)
  }
}
trans_array <- function(node,is.inner) {
  if(!is.null(node$value$type) && node$value$type == "empty") {
    if(is.inner) {
      list()
    } else {
      integer(0)
    }
  } else {
    values <- lapply(node$value,function(n) translate(n,T))
    checkTypes(values)
  }
}

translate <- function(node,is.inner=F) {
  switch(node$type,
    `object` = trans_object(node),
    `array` = trans_array(node,is.inner),
    `string` = node$value,
    `number` = node$value,
    `true` = T,
    `false` = F,
    `null` = NA,
  )
}


json.unpack <- function(string) {
  ret <- json_value()(streamParserFromString(string))
  if(ret$status == "ok") {
    translate(ret$node)
  } else {
    stop(ret)
  }
}
json.unpack.file <- function(filename) {
  ret <- json_value()(streamParserFromFileName(filename))
  if(ret$status == "ok") {
    translate(ret$node)
  } else {
    stop(ret)
  }
}
