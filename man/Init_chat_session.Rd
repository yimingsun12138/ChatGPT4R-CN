% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chat_API.R
\name{Init_chat_session}
\alias{Init_chat_session}
\title{Initialize a chat session for ChatGPT}
\usage{
Init_chat_session(
  global = NULL,
  model = "gpt-3.5-turbo-0301",
  import_histroy = NULL,
  temperature = 0.7,
  top_p = 1,
  n = 1,
  max_tokens = 2048,
  presence_penalty = 0,
  frequency_penalty = 0
)
}
\arguments{
\item{global}{Global system setting for chat session.}

\item{model}{OpenAI model used for chatting.}

\item{import_histroy}{Whether import a chat session? Set to a chat_session object if you would like to import chat history.}

\item{temperature}{What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. We generally recommend altering this or top_p but not both.}

\item{top_p}{An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10\% probability mass are considered.}

\item{n}{How many chat completion choices to generate for each input message.}

\item{max_tokens}{The maximum number of tokens allowed for the generated answer.}

\item{presence_penalty}{Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.}

\item{frequency_penalty}{Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.}
}
\value{
A function as a chat session.
}
\description{
OpenAI4R is designed to implement chat sessions in the form of functions, so that users can achieve continuous conversations by simply entering text into the function.
In order to provide multiple chat sessions in one environment, users need to initialize each chat session through this function.
It should be noted that OpenAI limits the length of dialogues, and when this limit is exceeded, the conversation will start deleting from the beginning one by one to meet the length limit.
Use \code{?chat_func_char} to get more information about chat sessions in function form.
}
\details{
\code{stream}, \code{logit_bias} and \code{user} parameter are not supported currently.
}
