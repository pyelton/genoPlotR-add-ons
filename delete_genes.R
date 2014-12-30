delete_genes <- function(list, gene_index){
    #Takes a list of dna_seg objects and removes genes by their index
    require(genoPlotR)
    require(ade4)

    for (i in 1:length(segs)){ 
        segs[[i]] = segs[[i]][-c(gene_index)]        
    } 
    segs
}