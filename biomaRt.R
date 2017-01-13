## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("biomaRt")

library(biomaRt)
ensembl = useMart("ensembl")
datasets <- listDatasets(ensembl)

human = useMart("ensembl", dataset = "hsapiens_gene_ensembl")

for (i in 1:nrow(datasets)) {
    ensembl <- datasets[i, 1]
    assign(paste0(ensembl), useMart("ensembl", dataset = paste0(ensembl)))
}

source("https://bioconductor.org/biocLite.R")
biocLite("EnsDb.Hsapiens.v79")

library(AnnotationDbi)
library(EnsDb.Hsapiens.v79)

# get all Ensembl gene IDs
human_EnsDb <- keys(EnsDb.Hsapiens.v79, keytype = "GENEID")

# get the biotype of each gene ID
human_gene_EnsDb <- ensembldb::select(EnsDb.Hsapiens.v79, keys = human_EnsDb, columns = "GENEBIOTYPE", keytype = "GENEID")

# and keep only protein coding genes
human_prot_coding_genes <- human_gene_EnsDb[which(human_gene_EnsDb$GENEBIOTYPE == "protein_coding"), ]


for (species in specieslist) {
    print(species)
    assign(paste0("homologs_human_", species), getLDS(attributes = c("ensembl_gene_id", "chromosome_name"), 
                                                      filters = "ensembl_gene_id", 
                                                      values = human_prot_coding_genes$GENEID, 
                                                      mart = human, 
                                                      attributesL = c("ensembl_gene_id", "chromosome_name"), 
                                                      martL = get(species)))
}

library(dplyr)

for (i in 1:length(specieslist)){
    species <- specieslist[i]
    homologs_human <- left_join(human_prot_coding_genes, get(paste0("homologs_human_", species)), by = c("GENEID" = "Ensembl.Gene.ID"))
    homologs_human[, paste0(species)] <- ifelse(is.na(homologs_human$Ensembl.Gene.ID.1), 0, 1)
    homologs_human <- homologs_human[, c(1, 6)]
    homologs_human <- homologs_human[!duplicated(homologs_human$GENEID), ]
    
    if (i == 1){
        homologs_human_table <- homologs_human
    } else {
        homologs_human_table <- left_join(homologs_human_table, homologs_human, by = "GENEID")
    }
}

homologs_human_table <- homologs_human_table[which(homologs_human_table[, grep("sapiens", colnames(homologs_human_table))] == 1), ]


gene_matrix <- homologs_human_table[, -1]

co_occurrence <- t(as.matrix(gene_matrix)) %*% as.matrix(gene_matrix)
co_occurrence[-grep("sapiens", rownames(co_occurrence)), -grep("sapiens", colnames(co_occurrence))] <- 0

genes <- data.frame(organism = colnames(gene_matrix),
                    number_genes = colSums(gene_matrix),
                    proportion_human_genes = colSums(gene_matrix)/nrow(homologs_human_table))


library(dendextend)
library(circlize)

# create a dendrogram
h <- hclust(dist(scale(as.matrix(t(gene_matrix))), method = "manhattan"))
dend <- as.dendrogram(h)

library(dplyr)
labels <- as.data.frame(dend %>% labels) %>%
    left_join(datasets, by = c("dend %>% labels" = "dataset")) %>%
    left_join(genes, by = c("dend %>% labels" = "organism"))

labels[, 2] <- gsub("(.*)( genes (.*))", "\\1", labels[, 2])
labels$group <- c(rep("mammal", 2), rep("fish", 8), "amphibia", rep("fish", 4), rep("bird", 5), rep("reptile", 2), rep("mammal", 41), "fungus", "lamprey", rep("seasquirt", 2), "nematode", "insect")

labels$description[grep("hedgehog", labels$description)] <- "Hedgehog Tenrec"
labels$description[grep("Saccharomyces", labels$description)] <- "Yeast"
labels$description[grep("savignyi", labels$description)] <- "C. savignyi"
labels$description[grep("intestinalis", labels$description)] <- "C. intestinalis"
labels$description[grep("elegans", labels$description)] <- "C. elegans"
labels$description[grep("turtle", labels$description)] <- "Turtle"
labels$description[grep("Vervet", labels$description)] <- "Vervet"

library(ggplot2)

my_theme <- function(base_size = 12, base_family = "sans"){
    theme_grey(base_size = base_size, base_family = base_family) +
        theme(
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.title = element_text(size = 14),
            panel.grid.major = element_line(color = "grey"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "aliceblue"),
            strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
            strip.text = element_text(face = "bold", size = 12, color = "navy"),
            legend.position = "bottom",
            legend.background = element_blank(),
            panel.margin = unit(.5, "lines"),
            panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
        )
}

labels_p <- labels[-1, ]

f = as.factor(labels_p[order(labels_p$proportion_human_genes, decreasing = TRUE), "description"])
labels_p <- within(labels_p, description <- factor(description, levels = f))

ggplot(labels_p, aes(x = description, y = proportion_human_genes, fill = group)) +
    geom_bar(stat = "identity") +
    my_theme() +
    labs(
        x = "",
        y = "Proportion of homology",
        fill = "",
        title = "Human gene homology",
        subtitle = "Proportion of human protein coding genes with homologs in 68 other species"
    )
