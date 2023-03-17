#' Make a completion request to the OpenAI API
#' 
#' @description Given a prompt, the model will return one or more predicted completions, and can also return the probabilities of alternative tokens at each position.
#' 
#' @param prompt_content The prompt to generate completions for, encoded as a string.
#' @param model OpenAI model used for completion.
#' @param suffix The suffix that comes after a completion of inserted text.
#' @param max_tokens The maximum number of tokens to generate in the completion.
#' @param temperature What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic.
#' @param top_p An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#' @param n How many completions to generate for the prompt.
#' @param logprobs Include the log probabilities on the logprobs most likely tokens, as well the chosen tokens. For example, if logprobs is 5, the API will return a list of the 5 most likely tokens. The API will always return the logprob of the sampled token, so there may be up to logprobs+1 elements in the response.
#' @param echo Whether echo back the prompt and the suffix in addition to the completion.
#' @param stop_char Up to 4 sequences where the API will stop generating further tokens. The returned text will not contain the stop sequence. Input as a vector.
#' @param presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
#' @param frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
#' @param best_of Generates best_of completions server-side and returns the "best" (the one with the highest log probability per token). When used with n, best_of controls the number of candidate completions and n specifies how many to return.
#' @param simplify Whether to output the completion directly or return the complete HTTP request content.
#' 
#' @details `stream`, `logit_bias` and `user` parameter are not supported currently.
#' 
#' @return completion in text format or a full HTTP request content.
#' 
#' @export
completion <- function(prompt_content,
                       model = 'text-davinci-003',
                       suffix = NULL,
                       max_tokens = 2048,
                       temperature = 0.7,
                       top_p = 1,
                       n = 1,
                       logprobs = NULL,
                       echo = TRUE,
                       stop_char = NULL,
                       presence_penalty = 0,
                       frequency_penalty = 0,
                       best_of = 1,
                       simplify = TRUE){
  
  #check prompt
  if(!(base::class(prompt_content) == 'character')){
    stop('prompt_content parameter can only be a string!')
  }
  if(base::length(prompt_content) > 1){
    stop('only one prompt is allowed!')
  }
  
  #check model
  if(!(model %in% OpenAI_model_list(simplify = TRUE))){
    stop('model is not supported by OpenAI!')
  }
  
  #check suffix
  if(!(base::is.null(suffix) | base::class(suffix) == 'character')){
    stop('suffix parameter must be NULL or a string!')
  }
  if(base::length(suffix) > 1){
    stop('only one suffix is allowed!')
  }
  
  #check best_of
  if(best_of < n){
    if(best_of == 1){
      warning('best_of must be greater than n, set to n by default!')
      best_of <- n
    }else{
      stop('best_of must be greater than n!')
    }
  }
  
  #check logprobs
  if(!(base::is.null(logprobs))){
    warning('HTTP request content will not be simplified if set the logprobs!')
    simplify <- FALSE
  }
  
  #check stop_char
  if(!(base::is.null(stop_char) | base::class(stop_char) == 'character')){
    stop('stop_char parameter can only be NULL or strings!')
  }
  
  #create HTTP body
  http_body <- base::list(`model` = model,
                          `prompt` = prompt_content,
                          `max_tokens` = max_tokens,
                          `temperature` = temperature,
                          `top_p` = top_p,
                          `n` = n,
                          `presence_penalty` = presence_penalty,
                          `frequency_penalty` = frequency_penalty,
                          `best_of` = best_of)
  
  #add suffix
  if(!(base::is.null(suffix))){
    http_body <- base::append(http_body,
                              base::list(`suffix` = suffix))
  }
  
  #add logprobs
  if(!(base::is.null(logprobs))){
    http_body <- base::append(http_body,
                              base::list(`logprobs` = logprobs))
  }
  
  #add stop
  if(!(base::is.null(stop_char))){
    http_body <- base::append(http_body,
                              base::list(`stop` = stop_char))
  }
  
  #request
  if(base::exists(x = 'OpenAI_organization')){
    request_POST <- httr::POST(
      url = URL_completion,
      httr::add_headers(`Content-Type` = 'application/json'),
      httr::add_headers(`Authorization` = base::paste('Bearer',OpenAI_API_key,sep = ' ')),
      httr::add_headers(`OpenAI-Organization` = OpenAI_organization),
      body = http_body,
      encode = 'json'
    )
  }else{
    request_POST <- httr::POST(
      url = URL_completion,
      httr::add_headers(`Content-Type` = 'application/json'),
      httr::add_headers(`Authorization` = base::paste('Bearer',OpenAI_API_key,sep = ' ')),
      body = http_body,
      encode = 'json'
    )
  }
  
  requset_content <- httr::content(x = request_POST)
  
  #check request error
  if(base::is.null(requset_content$choices)){
    stop(requset_content$error$message)
  }
  
  #whether simplify the request content?
  if(simplify){
    for(i in 1:n){
      if(echo){
        completion_content <- base::paste0('completion model:\n',
                                           prompt_content,
                                           base::gsub(pattern = '^\n\n',replacement = '',x = requset_content$choices[[i]]$text,fixed = FALSE),
                                           suffix,
                                           '\n\n')
      }else{
        completion_content <- base::paste0('completion model:\n',
                                           base::gsub(pattern = '^\n\n',replacement = '',x = requset_content$choices[[i]]$text,fixed = FALSE),
                                           '\n\n')
      }
      base::cat(completion_content)
    }
  }else{
    base::message('full http request content will be returned!')
    return(requset_content)
  }
  
}