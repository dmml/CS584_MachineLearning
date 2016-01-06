## read the data

data.miR.survival = read.table(file=input.dir %&% "input.txt",sep='\t', header=T, quote = "\"", stringsAsFactors = F)

##multiple pair-wise LR
phenolist=names(data.miR.survival)[706]
exprlist=names(data.miR.survival)[1:705]
results=data.frame()


for(cpheno in phenolist) {pheno = data.miR.survival[,cpheno]
                          for(cexpr in exprlist) {expr = data.miR.survival[,cexpr]
                                                  fit = lm(pheno ~ expr)
                                                  sumfit=summary(fit)   
                                                  results = rbind( results, data.frame(cexpr, cpheno, coef(sumfit) ) )                      
                                                  print(cpheno)
                                                  print(cexpr)                     
                                                  print(summary(fit))}}
write.table(results, file="results.txt", quote=F, col=T, row=F)
