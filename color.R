color <- function(list, annot, color = "gray"){
    #Takes a list of dna_seg objects, looks for all annotations containing annot and changes the color of those genes to color
    require(genoPlotR)
    require(ade4)

    for (i in 1:length(list)){ 
        x <- list[[i]][grep(annot, list[[i]]$annotation, perl=TRUE),]
        if (dim(x)[1] == 0){
        }else{
            list[[i]][grep(annot, list[[i]]$annotation, perl=TRUE),]$col = color
        }
        
    } 
    list
}
