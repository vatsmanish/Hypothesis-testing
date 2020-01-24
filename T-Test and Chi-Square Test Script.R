setwd("C:\\Users\\MANDY\\Desktop\\dataset")
data=read.csv("attrition2.csv")
{test = function(data,target){
    #Check whether the passed data is in dataframe format
    if (!is.data.frame(data)) {
        stop("Pass the data as a dataframe")
    }
    #Check whether the passed target is part of the dataframe
    if (!target %in% colnames(data)) {
        stop("The target that you have passed is not part of the dataframe")
    }
    #Check the type of the data frame it should be binary classifier or categorical
    
    #Subset the data without the target
    keep = colnames(data) %in% c(target)
    X <- data[,!keep]
    y <- data[,keep]
    
    #If the data is numeric calculate the t-test p value and store it in a vector
    vect_t=c()
    vect_c=c()
    count_t=1
    count_c=1
    for (i in 1:ncol(X)) {
        if (is.numeric(X[,i])) {
            
            t=t.test(X[,i] ~ y)
            vect_t[count_t]=t$p.value
            print(paste("t",t$p.value))
            count_t=count_t+1
        }else{
            c=chisq.test(X[,i], y)
            vect_c[count_c]=c$p.value
            print(paste("c",c$p.value))
            count_c = count_c+1
        }
    }
    #If the data is categorical calculate the chi-
    #square test p-value and store it in a vector
    return(vect_t)
}
    
}

test(data,"Attrition")
View(data)

