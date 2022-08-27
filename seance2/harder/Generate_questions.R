library("svglite")
library("yaml")
source("../../library/simple_2_mulitple.R")
yaml::read_yaml("bank.yaml")
n.question = 100

base_list = list("pictures base directory" =  "answer_plot",
                 "categories" = list(list(
                              "name" = "seance2",
                              questions = list())
                 )
                 )

for(i in 1:n.question){
  set.seed(i)
  svglite(paste0("answer_plot/Q_",i,".svg"), width = 12, height = 8)
  question = Generate_multiple_question(max.obs = 5)
  base_list$categories[[1]]$questions[[i]] = list()
  base_list$categories[[1]]$questions[[i]]$name = paste0("Question ", i) 
  base_list$categories[[1]]$questions[[i]]$class = "Numerical"
  base_list$categories[[1]]$questions[[i]]$statement =base::enc2utf8(question$question)
  base_list$categories[[1]]$questions[[i]]$solution = list(value = question$response,
                                                           error = 0.1)
  base_list$categories[[1]]$questions[[i]]$feedback = paste0("Q_",i,".svg\n")
  dev.off()
}

yaml::write_yaml(base_list,file = "bank.yaml")


