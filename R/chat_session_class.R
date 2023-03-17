#' chat_session class
#' 
#' @description chat_session class is used for prompting messages to ChatGPT model and archiving chat history.
setClass(Class = 'chat_session',slots = c(session_id = 'character',history = 'list',archive = 'list'))

#' Initialize chat_session class
#' 
#' @description Initialize chat_session class.
setMethod(f = 'initialize',
          signature = methods::signature(.Object = 'chat_session'),
          definition = function(.Object,session_id = NULL,global = NULL){
            
            #check parameter
            if(!(base::is.null(global) | base::class(global) == 'character')){
              stop('system global parameter must be NULL or a string!')
            }
            
            #initialize
            .Object@session_id <- session_id
            .Object@history <- base::list(base::list(`role` = 'system',`content` = global))
            .Object@archive <- .Object@history
            
            #return
            return(.Object)
          })

#' Filter chat_session history if necessary
#' 
#' @description Filter chat_session history if necessary.
setGeneric(name = 'filter_chat_history',def = function(.Object,force){base::standardGeneric(f = 'filter_chat_history')})

setMethod(f = 'filter_chat_history',
          signature = methods::signature(.Object = 'chat_session'),
          definition = function(.Object,force){
            
            #filter NULL content
            if(.Object@history[[1]]$role == 'system' & base::is.null(.Object@history[[1]]$content)){
              .Object@history <- utils::tail(x = .Object@history,n = -1)
            }
            
            #force filter
            if(force){
              .Object@history <- utils::tail(x = .Object@history,n = -1)
            }
            
            #return
            return(.Object)
          })

#' Add chat_session history
#' 
#' @description Add chat_session history.
setGeneric(name = 'add_chat_history',def = function(.Object,role,prompt_content){base::standardGeneric(f = 'add_chat_history')})

setMethod(f = 'add_chat_history',
          signature = methods::signature(.Object = 'chat_session'),
          definition = function(.Object,role,prompt_content){
            
            #check parameter
            if(!(role %in% c('system','user','assistant'))){
              stop('role can only be system, user or assistant!')
            }
            if(base::class(prompt_content) != 'character'){
              stop('prompt content can only be a string!')
            }
            
            #add prompt
            .Object@history <- base::append(.Object@history,base::list(base::list(`role` = role,`content` = prompt_content)))
            .Object@archive <- base::append(.Object@archive,utils::tail(x = .Object@history,n = 1))
            
            #return
            return(.Object)
          })

#' Export chat_session archive as chat history
#' 
#' @description Export chat_session archive as chat history.
setGeneric(name = 'export_chat_history',def = function(.Object){base::standardGeneric(f = 'export_chat_history')})

setMethod(f = 'export_chat_history',
          signature = methods::signature(.Object = 'chat_session'),
          definition = function(.Object){
            chat_history <- base::lapply(X = .Object@archive,FUN = function(x){
              prompt_content <- base::paste(x$role,x$content,sep = ' : \n')
              return(prompt_content)
            })
            chat_history <- base::do.call(what = base::paste,args = base::list(chat_history,collapse = '\n\n'))
            base::cat(chat_history)
          })