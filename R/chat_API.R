#' Generate chat_session id.
generate_session_id <- function(){
  
  #generate session id
  indi <- TRUE
  while(indi){
    session_id <- base::paste('session',uuid::UUIDgenerate(),sep = ':')
    temp <- base::ls(envir = base::globalenv())
    if(!(session_id %in% temp)){
      indi <- FALSE
    }
  }
  
  #return
  return(session_id)
}

#' Initialize a chat session for ChatGPT.
#' 
#' @param global Global system setting for chat session.
#' @param model OpenAI model used for chatting.
#' @param import_histroy Whether import a chat session? Set to a chat_session object if you would like to import chat history.
#' @param temperature What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. We generally recommend altering this or top_p but not both.
#' @param top_p An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#' @param n How many chat completion choices to generate for each input message.
#' @param max_tokens The maximum number of tokens allowed for the generated answer.
#' @param presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
#' @param frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
#' 
#' @return A function as a chat session.
#' @export
Init_chat_session <- function(global = NULL,
                              model = 'gpt-3.5-turbo-0301',
                              import_histroy = NULL,
                              temperature = 0.7,
                              top_p = 1,
                              n = 1,
                              max_tokens = 2048,
                              presence_penalty = 0,
                              frequency_penalty = 0){
  
  #check parameter
  if(!(base::is.null(global) | base::class(global) == 'character')){
    stop('global parameter must be NULL or a string!')
  }
  if(!(model %in% OpenAI_model_list(simplify = TRUE))){
    stop('model is not supported by OpenAI!')
  }
  if(!(base::is.null(import_histroy) | base::class(import_histroy) == 'chat_session')){
    stop('import_histroy must be a chat_session object if you would like to import!')
  }
  
  #Initialize chat_session
  session_id <- generate_session_id()
  if(base::is.null(import_histroy)){
    base::assign(x = session_id,value = methods::new('chat_session',session_id = session_id,global = global),envir = .GlobalEnv)
  }else{
    base::assign(x = session_id,value = import_histroy,envir = .GlobalEnv)
  }
  
  #generate chat function
  temp <- chat_func_char
  temp <- base::sprintf(temp,
                        model,temperature,top_p,n,max_tokens,presence_penalty,frequency_penalty,
                        session_id,session_id,session_id)
  
  base::eval(expr = base::parse(text = temp))
  
  #return
  return(chat_func)
}