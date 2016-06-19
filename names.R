# name
# A <- c("Anthony Keith Gwynn", "Michael Chien Shih", "Ichiro Suzuki")

FML_name <- function(x){
        C <- strsplit(x, " ")
        data <- data.frame()
        counter <- 0
        pb <- txtProgressBar(min = 0, max = length(C), style = 3)
        for (i in (1:length(C))){
                if (length(C[[i]]) == 2){
                        A <- c(C[[i]][1], "", C[[i]][2]) 
                        data <- rbind(data, A, stringsAsFactors = F)
                        counter <- counter + 1
                        setTxtProgressBar(pb, counter)
                } else {
                        data <- rbind(data, C[[i]], stringsAsFactors = F)
                        counter <- counter + 1
                        setTxtProgressBar(pb, counter)
                }
        }
        names(data) <- c("First", "Middle", "Last")
        return(data)
}

middle_abbre <- function(x){
        mid <- x$Middle
        mid <- substr(mid, 1, 1)
        counter <- 0
        pb <- txtProgressBar(min = 0, max = length(mid), style = 3)
        name_list <- c()
        y <- 1
        for (i in mid){
                if (nchar(i) == 1){
                        mid_ab <- paste(i, ".", sep = "")
                        name_list <- c(name_list, paste(x[y, 1], mid_ab, x[y, 3]))
                } else {
                        name_list <- c(name_list, paste(x[y, 1], x[y, 3]))
                       
                }
                y <- y + 1
                counter <- counter + 1
                setTxtProgressBar(pb, counter)
        }
        
        return(name_list)
}
 
# namelist <- FML_name(A)
# namelist
# namelist.m <- middle_abbre(namelist)
# namelist.m
        