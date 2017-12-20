rule<-read.csv(file.choose(), header =T) # 위에 있는 항목 이름을 표시할때 header = T
#rule <- readLines(file.choose())
head(rule)
attach(rule)
a1
table(a1)
table(Gender) # 데이터가 없는 곳은 subset에서 제외한 후 다시 진행
test <- subset(rule, select=c("a35", "a36","a37","a38","a41","a42",  
                              "a43","a44","a45","a46","a47","a48","a49","a50",
                              "a51","a53","a54","a55"))
head(test)
## 요인분석
fit <- factanal(test, factors=5, rostation="varimax")
print(fit, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만
e_value<- eigen(cor(test))
e_value # 결과값 맨 위에 $values 부분에서 1.0밑으로 내려가기전까지만 묶어서 한다.
# a40과 같이 2개 이상의 요소에 걸릴 경우 한가지라도 0.6이상이라면 그것을 지키고
# 둘다 0.6이 안넘는다면 지워야 한다. --> 설명력을 높이기 위해서

test2 <- subset(rule, select=c("a17", "a18","a19","a20","a21","a22",  
                              "a23","a24","a25","a26","a27","a28","a29","a30",
                              "a31","a32","a33","a34"))
head(test2)
## 요인분석
fit1 <- factanal(test2, factors=4, rostation="varimax")
print(fit1, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만
e_value<- eigen(cor(test2))
e_value


test3 <- subset(rule, select=c("a1", "a2","a3","a4","a6",  
                               "a7","a8","a10","a12","a13","a14"))
head(test3)
## 요인분석
fit2 <- factanal(test3, factors=6, rostation="varimax")
print(fit2, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만
e_value<- eigen(cor(test3))
e_value
