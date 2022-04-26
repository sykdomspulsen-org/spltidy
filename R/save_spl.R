#' Saves (serializes) an encrypted object to disk
#' @param x The object to serialize
#' @param filepath The filepath (must end in '.spl.encrypted' or '.spl')
#' @param nthreads Number of threads to use. Default 1.
#' @param public_key_path Path to public key
#' @export
save_spl <- function(x, filepath, nthreads = 1, public_key_path = Sys.getenv("ENCRYPTR_ID_RSA_PUB")){
  UseMethod("save_spl", x)
}

#' @method save_spl default
#' @export
save_spl.default  <- function(x, filepath, nthreads = 1, public_key_path = Sys.getenv("ENCRYPTR_ID_RSA_PUB")){
  if(!(stringr::str_detect(filepath, ".spl.encrypted$") | stringr::str_detect(filepath, ".spl$"))){
    stop("filepath must end with '.spl.encrypted' or '.spl'")
  }

  if(is.null(public_key_path)){
    public_key_path <- system.file("splsave/id_rsa.pub", package="spltidy")
  } else if(public_key_path==""){
    public_key_path <- system.file("splsave/id_rsa.pub", package="spltidy")
  }

  if(stringr::str_detect(filepath, ".spl$")){
    qs::qsave(
      x = x,
      file = filepath,
      nthreads = nthreads
    )
    message("Saved to *UN*encrypted ", filepath)

  } else {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    qs::qsave(
      x = x,
      file = tmp,
      nthreads = nthreads
    )

    openssl::encrypt_envelope(tmp, public_key_path) %>%
      saveRDS(file = filepath, compress = FALSE)

    message("Saved to ", filepath, ", ENCRYPTED with public key ", public_key_path)
  }
}

#' @method save_spl splfmt_rts_data_v1
#' @export
save_spl.splfmt_rts_data_v1  <- function(x, filepath, nthreads = 1, public_key_path = Sys.getenv("ENCRYPTR_ID_RSA_PUB")){
  x <- as.data.frame(x)
  attr(x, "spl_class") <- "splfmt_rts_data_v1"

  # call save_spl.default
  NextMethod()
}

#' Reads an object in an encrypted file serialized to disk
#' @param filepath The filepath (must end in '.spl.encrypted' or '.spl')
#' @param private_key_path Path to private key
read_spl <- function(filepath, private_key_path = Sys.getenv("ENCRYPTR_ID_RSA")){
  if(!(stringr::str_detect(filepath, ".spl.encrypted$") | stringr::str_detect(filepath, ".spl$"))){
    stop("filepath must end with '.spl.encrypted' or '.spl'")
  }

  if(stringr::str_detect(filepath, ".spl$")){
    retval <- qs::qread(filepath)
  } else if (stringr::str_detect(filepath, ".spl.encrypted$")){
    tmp <- tempfile()
    on.exit(unlink(tmp))

    .crypt = readRDS(filepath)
    zz = file(tmp, "wb")
    openssl::decrypt_envelope(
      .crypt$data,
      .crypt$iv,
      .crypt$session,
      key = private_key_path,
      password = NULL
    ) %>%
      writeBin(zz)
    close(zz)

    retval <- qs::qread(tmp)
    retval
  }

  if(!is.null(attr(retval, "spl_class"))){
    if(attr(retval, "spl_class") == "splfmt_rts_data_v1"){
      setDT(retval)
      set_splfmt_rts_data_v1(retval)
    }
  }
  return(retval)
}
