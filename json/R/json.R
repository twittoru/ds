Json <- setRefClass("Json",
  fields = c("packer", "unpacker"),
  methods = list(
    initialize = function() {
      packer <<- new("packer")
      unpacker <<- new("unpacker")
    },
    pack = function(object) {
      packer$pack(object)
    },
    unpack = function(msg) {
      unpacker$unpack(msg)
    }
  )
)
