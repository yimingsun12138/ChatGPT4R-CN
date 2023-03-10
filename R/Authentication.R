#' Set up authentication using OpenAI API key.
#' 
#' @param key Your OpenAI API key.
#' @param organization Usage from the API requests will count against the specified organization's subscription quota. Set to null as default.
#' @param return_list Whether return the OpenAI available model list?
#' 
#' @export
Auth_OpenAI <- function(key,organization = NULL,return_list = FALSE){
  
  #get model list
  if(base::is.null(organization)){
    model_list <- httr::GET(
      url = URL_models,
      httr::add_headers(`Authorization` = base::paste('Bearer',key,sep = ' '))
    )
  }else{
    model_list <- httr::GET(
      url = URL_models,
      httr::add_headers(`Authorization` = base::paste('Bearer',key,sep = ' ')),
      httr::add_headers(`OpenAI-Organization` = organization)
    )
  }
  model_list <- httr::content(model_list)
  
  #check accessibility of API_key and organization
  if(base::is.null(model_list$error)){
    if(return_list){
      return(model_list)
    }else{
      base::assign(x = 'OpenAI_API_key',value = key,envir = .GlobalEnv)
      if(!base::is.null(organization)){
        base::assign(x = 'OpenAI_organization',value = organization,envir = .GlobalEnv)
      }
      base::message('All settled!')
    }
  }else{
    stop(model_list$error$message)
  }
}

#' Get OpenAI available model list.
#' 
#' @param simplify Whether simplify the list returned?
#' 
#' @return A character vector or a complex character list.
#' @export
OpenAI_model_list <- function(simplify = TRUE){
  
  #check accessibility of API_key
  if(!base::exists(x = 'OpenAI_API_key')){
    stop('First verify your OpenAI API key using Auth_OpenAI!')
  }
  
  #get model list
  if(base::exists(x = 'OpenAI_organization')){
    model_list <- Auth_OpenAI(key = OpenAI_API_key,organization = OpenAI_organization,return_list = TRUE)
  }else{
    model_list <- Auth_OpenAI(key = OpenAI_API_key,organization = NULL,return_list = TRUE)
  }
  
  #simplify
  if(simplify){
    temp <- model_list$data
    temp <- base::unlist(x = base::lapply(X = temp,FUN = function(x){
      return(x$id)
    }))
    return(temp)
  }else{
    return(model_list)
  }
}