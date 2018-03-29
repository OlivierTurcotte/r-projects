## J'avais de bonne intentions, j'ai un résultat, seulement je sais pas trop 
## c'est quoi... 


cptDonnes <- function(m,shift,loi, parameters, p){
    
    x <- numeric(0) 
    
    for (i in seq_len(m)){
        for(j in seq_along(p)){
            p[[j]] <- p[[j]] + i*shift[j]
        }
        p$n <- parameters$n
        x_temp <- do.call(loi,p)
        x <- append(x,x_temp)
    }
    x
}

genDonnees <- function(){
    library(actuar)
    
    lois <- list(Exponentielle = "exp" , Uniforme = "unif" , Pareto = "pareto")
    chLoi <- menu(names(lois),title = "Choisis une loi de probabilité")
    loi <- lois[[chLoi]]
    f <- paste("r",loi,sep = "")
    
    params <- formals(f)
    name_params <- names(params)
    temp <- ""
    
    for(i in seq_along(name_params)){
        while(temp == ""){
            temp <- readline(paste(name_params[i],"="))
        }
        params[i] <- as.numeric(temp)
        temp <- ""
    }
    
    accept <- F
    shift <- numeric(0)
    
    g <- paste("d",loi,sep = "")
    n2_param <- name_params[name_params != "n"]
    p <- params[n2_param] # On enlève le param n
    
    
    while(accept != T){
        m <- readline("m =")
        for(i in 1:(length(name_params)-1)){
            shift[i] <- as.numeric(readline(paste(name_params[i+1],"shift =")))
        }
        temp <- cptDonnes(m,shift,f,params,p)
        hist(temp, probability = T)
        curve(do.call(g,list(x,unlist(unname(p)))),ylab = "y",add = T, col = "red")
        legend(0.5, legend=c("Supposé"),
               col=c("red"), lty=1:2, cex=0.8)
        accept <- menu(c("Yes","No"), title = "Accept ?")
    }
    temp
}
genDonnees()


