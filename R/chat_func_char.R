#' ChatGPT function written in string for dynamic parsing.
#' 
#' @description The goal of OpenAI4R is to wrap the OpenAI API in R, allowing for simple and intuitive, even elegant calls.
#' For the ChatGPT model used for interactive conversations, an intuitive way to use it is to initialize a function as a chat session and simply input text each time to complete the conversation.
#' To make the conversation flow smoother, the conversation function should default to all other parameters, only requiring input for the conversation content.
#' Therefore, it is necessary to dynamically initialize the chat session when there are multiple chat sessions in the current environment.
#' 
#' @details Below is an introduction to the roles and functions of different parameters in the function-based chat session.
#' @details `prompt_content` The conversation content that you input.
#' @details `role` The identity used when issuing the instructions, can only be selected as 'user' or 'system'.
#' @details `model` The AI model provided by OpenAI.
#' @details `temperature`,`top_p`,`n`,`max_tokens`,`presence_penalty`,`frequency_penalty` Refer to the parameter description from OpenAI.
#' @details `stop_char` Up to 4 sequences where the API will stop generating further tokens. Input as a vector.
#' @details `simplify` Whether to output the conversation directly or return the complete HTTP request content.
#' @details `export_history` Whether export the chat history as text.
#' @details `export_chat_session` Whether export the chat history as chat_session object.
#' 
#' @export
chat_func_char <- "chat_func <- function(prompt_content,
                      role = 'user',
                      model = '%s',
                      temperature = %f,
                      top_p = %f,
                      n = %d,
                      max_tokens = %d,
                      presence_penalty = %f,
                      frequency_penalty = %f,
                      stop_char = NULL,
                      simplify = TRUE,
                      export_history = FALSE,
                      export_chat_session = FALSE){
  
  #check role
  if(!(role %%in%% c('system','user'))){
    stop('prompt role can only be system or user!')
  }
  
  #check prompt
  if(!(base::class(prompt_content) == 'character')){
    stop('prompt_content parameter can only be a string!')
  }
  if(base::length(prompt_content) > 1){
    stop('only one prompt is allowed!')
  }
  
  #check stop_char
  if(!(base::is.null(stop_char) | base::class(stop_char) == 'character')){
    stop('stop_char parameter can only be NULL or strings!')
  }
  
  #whether export chat_session object
  if(export_chat_session){
    base::message('chat_session object will be returned!')
    return(`%s`)
  }
  
  #whether export history
  if(export_history){
    base::message('chat history text will be returned!')
    return(export_chat_history(.Object = `%s`))
  }
  
  #add prompt
  temp_session <- add_chat_history(.Object = `%s`,role = role,prompt_content = prompt_content)
  
  #filter history while processing http POST
  indi <- TRUE
  forcement <- FALSE
  
  while(indi){
    #filter chat history
    temp_session <- filter_chat_history(.Object = temp_session,force = forcement)
    chat_history <- temp_session@history
    
    #create http body
    http_body <- base::list(`model` = model,
                            `temperature` = temperature,
                            `top_p` = top_p,
                            `n` = n,
                            `max_tokens` = max_tokens,
                            `presence_penalty` = presence_penalty,
                            `frequency_penalty` = frequency_penalty,
                            `messages` = chat_history)
    
    #add stop
    if(!(base::is.null(stop_char))){
      http_body <- base::append(http_body,
                                base::list(`stop` = stop_char))
    }
    
    #request
    if(base::exists(x = 'OpenAI_organization')){
      request_POST <- httr::POST(
        url = URL_chat,
        httr::add_headers(`Content-Type` = 'application/json'),
        httr::add_headers(`Authorization` = base::paste('Bearer',OpenAI_API_key,sep = ' ')),
        httr::add_headers(`OpenAI-Organization` = OpenAI_organization),
        body = http_body,
        encode = 'json'
      )
    }else{
      request_POST <- httr::POST(
        url = URL_chat,
        httr::add_headers(`Content-Type` = 'application/json'),
        httr::add_headers(`Authorization` = base::paste('Bearer',OpenAI_API_key,sep = ' ')),
        body = http_body,
        encode = 'json'
      )
    }
    
    requset_content <- httr::content(x = request_POST)
    
    #check request error
    if(base::is.null(requset_content$choices) & base::is.null(requset_content$error$code)){
      stop(requset_content$error$message)
    }
    
    #length exceeded?
    if(!(base::is.null(requset_content$choices))){
      indi <- FALSE
      forcement <- FALSE
    }else if(base::is.null(requset_content$choices) & requset_content$error$code == 'context_length_exceeded'){
      indi <- TRUE
      forcement <- TRUE
    }else{
      stop(requset_content$error$message)
    }
  }
  
  #process content
  for(i in 1:n){
    temp_session <- add_chat_history(.Object = temp_session,
                                     role = requset_content$choices[[i]]$message$role,
                                     prompt_content = base::gsub(pattern = '^\\n\\n',replacement = '',x = requset_content$choices[[i]]$message$content,fixed = FALSE))
  }
  
  #export to global env
  base::assign(x = '%s',value = temp_session,envir = .GlobalEnv)
  
  #whether simplify returned content
  if(simplify){
    for(i in 1:n){
      base::cat(base::paste0('ChatGPT:\\n',base::gsub(pattern = '^\\n\\n',replacement = '',x = requset_content$choices[[i]]$message$content,fixed = FALSE),'\\n\\n'))
    }
  }else{
    base::message('full http request content will be returned!')
    return(requset_content)
  }
  
}"