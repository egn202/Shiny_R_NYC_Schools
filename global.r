nydoe = read.csv('nydoe.csv')
nydoe$District=as.factor(nydoe$District)
nydoe$School.Number=as.factor(nydoe$School.Number)

scatter1 = nydoe %>% select(Math.Std,ELA.Std,RI.Score,CollabT.Score, SuprtEnv.Score,Leadrshp.Score,Comunty.Score,Trust.Score)
colnames(scatter1)[1:8] = c("Math","ELA","Rigorous Instruction","Collaborative Teachers","Supportive Enviornment","Leadership","Community","Trust")
scatter1 = scatter1 %>% pivot_longer(c(3:8), names_to = "Quality",values_to = "QScore" ) %>% pivot_longer(c(1,2), names_to = "Test", values_to = "TScore")
