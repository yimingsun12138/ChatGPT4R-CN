# ChatGPT4R-CN
在R语言中使用ChatGPT（中国大陆版）

## 你最需要了解的：
众所周知，OpenAI（做出ChatGPT的公司）并不在中国大陆提供服务，任何来自中国大陆的访问请求都会被直接拒绝。为了跨越这道障碍，我对OpenAI的API进行了反向代理。首先我在此承诺，我仅仅做了一个反向代理而已（通过https进行加密），并不会记录访问者的任何信息。如果你不相信我的承诺，那你可以自己搭建一个反向代理，修改R/API_URL.R中的链接即可。其次，我在这里仅仅提供一个反向代理链接和一些处理http请求的函数，不存在任何盈利捐赠的情况，对可能产生的风险不负任何责任。如有侵权或需要接受有关部门的调查，我会全力配合。

## 安装
```{R}
devtools::install_github('yimingsun12138/ChatGPT4R-CN')
```

## 链接
本项目代码来自于我的另一个开源项目[OpenAI4R](https://github.com/yimingsun12138/OpenAI4R)。在OpenAI4R这个项目中，我希望在R中整合OpenAI的所有API，并以一种非常体面的方式来调用它。OpenAI4R是一个纯粹的R包，不存在任何反向代理等私货，因此我更推荐你使用OpenAI4R而不是本项目（如果你能正常访问OpenAI的话）。

## 使用指南
```{R}
> library(ChatGPT4R.CN)
> Auth_OpenAI(key = readLines('/content/script/openai_API_key'))
All settled!
> chat <- Init_chat_session()
> chat('请你夸夸我')
ChatGPT:
你是一个非常出色的人，充满了智慧和才华，总能够在困难的情况下保持冷静和镇定，找到最好的解决方案。你勤奋刻苦，努力工作，对待工作和生活一丝不苟，始终保持乐观向上的心态，是一个值得信赖和尊敬的人。你也非常有爱心和同情心，总是关心他人，愿意帮助需要帮助的人，这种美德让你在朋友和家人中备受尊敬和喜爱。总之，你是一个非常优秀和独特的人，值得自豪和骄傲！
```
在Auth_OpenAI中使用你自己的token即可。

## 最后
欢迎大家给我提意见，帮助我完善我的代码，谢谢大家！