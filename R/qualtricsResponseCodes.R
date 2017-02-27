# Helper function. Checks responses against qualtrics response codes.

qualtRicsResponseCodes <- function(res, raw=FALSE) {
  # Check status code and raise error/warning
  if(res$status_code == 200) {
    return(list(
      "content" = ifelse(raw, content(res, "raw"), content(res)),
      "OK" = TRUE
      )
    )
  } else if(res$status_code == 401) {
    stop("Qualtrics API raised an authentication (401) error - you may not have the required authorization. Please check your API key and root url.")
  } else if(res$status_code == 400) {
    stop("Qualtrics API raised a bad request (400) error - Please report this on https://github.com/JasperHG90/qualtRics/issues")
  } else if(res$status_code == 404) {
    stop("Qualtrics API complains that the requested resource cannot be found (404 error). Please check if you are using the correct survey ID.")
  } else if(res$status_code == 500) {
    warning(paste0("Qualtrics API reports an internal server (500) error. Please contact Qualtrics Support (https://www.qualtrics.com/contact/) and provide the instanceId and errorCode below.", "\n",
                   "\n",
                   "instanceId:", " ", content(res)$meta$error$instanceId, "\n",
                   "errorCode: ", content(res)$meta$error$errorCode))
    return(list(
      "content" = content(res),
      "OK"= FALSE
      )
    )
  } else if(res$status_code == 503) {
    warning(paste0("Qualtrics API reports a temporary internal server (500) error. Please contact Qualtrics Support (https://www.qualtrics.com/contact/) with the instanceId and errorCode below or retry your query.", "\n",
                   "\n",
                   "instanceId:", " ", content(res)$meta$error$instanceId, "\n",
                   "errorCode: ", content(res)$meta$error$errorCode))
    return(list(
      "content" = content(res),
      "OK"= FALSE
    )
    )
  } else if(res$status_code == 413) {
    stop("The request body was too large. This can also happen in cases where a multipart/form-data request is malformed.")
  } else if(res$status_code == 429) {
    stop("You have reached the concurrent request limit.")
  }
}
