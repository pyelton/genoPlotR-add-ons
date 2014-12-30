flip  = function (dna_seg) {
    #flips gene cluster around
    require(genoPlotR)
    require(ade4)

    v = sort(c(dna_seg$start, dna_seg$end))
    d = (dna_seg$start - dna_seg$end) < 0
    
    for (i in 1:length(dna_seg$start)){
        if (d[i] == T){
            d[i] <- 1
        }else{
            d[i] <- -1
        }
    }
    dna_seg$strand = d #Sets the new directions for each gene
    for (j in 1:dim(dna_seg)[1]) { #Sorts starts and stops
        start = as.integer(sort(dna_seg[j,2:3])[1])
        end = as.integer(sort(dna_seg[j,2:3])[2])
        dna_seg$start[j] = start
        dna_seg$end[j] = end
    }
    #Finds gene lengths
    dna_seg$length = abs(dna_seg$end - dna_seg$start)
    #Flips start of the cluster
    dna_seg$start[1] = v[length(v)]
    inter = c()
    z = length(dna_seg$start)-1
    for (x in 1:length(dna_seg$start)){
        #Finds intergenic distances
        int = dna_seg$start[x+1] - dna_seg$end[x]
        inter = append(inter, int)
    }   
    for (i in 2:length(dna_seg$start)){ #Sets the new starts and stops
        if (dna_seg$start[1] < dna_seg$start[length(v)/2]){
            dna_seg$end[i-1] = dna_seg$start[i-1] + dna_seg$length[i-1]
            dna_seg$start[i] = dna_seg$end[i-1] + inter[i-1]
            dna_seg$end[i] = dna_seg$start[i] + dna_seg$length[i]
        } else {   
            dna_seg$end[i-1] = dna_seg$start[i-1] - dna_seg$length[i-1]
            dna_seg$start[i] = dna_seg$end[i-1] - inter[i-1]
            dna_seg$end[i] = dna_seg$start[i] - dna_seg$length[i]
        }
    } 
    dna_seg
}
