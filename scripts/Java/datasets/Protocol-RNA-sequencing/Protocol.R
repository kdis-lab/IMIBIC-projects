# External tools needed. Before starting this protocol, make sure the following external tools are installed and callable from 
# the command promt in your system
# 1- tophat2: An aligner to map short reads to a genome
# 2- IGV: A tool to visualize alignment files (http://www.broadinstitute.org/software/igv/log-in) *Optional
# 3- samtools program (http://www.htslib.org/) for manipulation of SAM- and BAM-formatted file
# 4- HTSeq package (http://htseq.readthedocs.io/en/release_0.9.1/) for counting of mapped reads. Python 2 is needed to execute this package.
# 5- bowtie2

#Input file formats

# In general, the starting point is a collection of FASTQ files, the commonly used 
# format for reads from Illumina sequencing machines.

# R packages needed
wants <- c("ShortRead", "edgeR", "DESeq", "GenomicRanges", "GenomicFeatures", "org.Dm.eg.db")

has   <- wants %in% rownames(installed.packages())

# Install the libreries if is necessary
if(any(!has)) {
  source("http://www.Bioconductor.org/biocLite.R")
  biocLite("BiocUpgrade")
  biocLite(wants)}

library(ShortRead)

# Step: Sequence quality checks

fqQC = qa(dirPath = ".", pattern = ".fastq$", type = "fastq")
report(fqQC, type = "html", dest = "fastqQAreport")

# Step: Collect metadata for experiment

# Collapse the initial table (sri) to one row per sample:

sri$LibraryName = gsub("S2_DRSC_","",sri$LibraryName) # trim label
samples = unique(sri[,c("LibraryName","LibraryLayout")])

for(i in seq_len(nrow(samples))) {

  rw = (sri$LibraryName == samples$LibraryName[i]) 
  
  if(samples$LibraryLayout[i] == "PAIRED") { 
    samples$fastq1[i] = paste0(sri$Run[rw],"_1.fastq",collapse = ",") 
    samples$fastq2[i] = paste0(sri$Run[rw],"_2.fastq",collapse = ",") 
    } 
  else {
    samples$fastq1[i] = paste0(sri$Run[rw],".fastq",collapse = ",")
    samples$fastq2[i] = ""
  }  
}

# Add important or descriptive columns to the metadata table (experimental groupings are set on the basis of the 
# 'LibraryName' column, and a label is created for plotting)

samples$condition = "CTL"
samples$condition[grep("RNAi",samples$LibraryName)] = "KD"
samples$shortname = paste(substr(samples$condition,1,2), substr(samples$LibraryLayout,1,2), seq_len(nrow(samples)), 
                          sep = ".")

# As the downstream statistical analysis of differential expression relies on this table, carefully inspect (and correct, 
# if necessary) the metadata table. In particular, verify that there exists one row per sample, that all columns of 
# information are populated and that the file names, labels and experimental conditions are correct.

samples

# Step: Mapping reads, organize files, inspect mapping

# Align the reads (using tophat2) to the reference genome
#By using R string manipulation, construct the Unix commands to call tophat2.

# loop through the set of samples and construct the full tophat2 command)

# tophat2 creates a directory for each
# sample with the mapped reads in a BAM file, called accepted_hits.bam. Note that BAM files, and equivalently 
#SAM files (an uncom-pressed text version of BAM), are the de facto standard file for alignments.

gf = "Drosophila_melanogaster.BDGP5.70.gtf"
bowind = "Dme1_BDGP5_70"
cmd = with(samples, paste("tophat2 -G", gf, "-p 5 -o", LibraryName, bowind, fastq1, fastq2))
system(cmd)

# Organize, sort and index the BAM files and create SAM files
# Organize the BAM files into a single directory, sort and index them and create SAM files by running the following  R-generated commands

# The set of files containing mapped reads (from tophat2, accepted_hits.bam) (typi-cally) needs to be transformed before it can be used with other
# downstream tools. In particular, the samtools command is used to prepare variations of the mapped reads. Specifically, 
# a sorted and indexed version of the BAM file is created, which can be used in genome browsers such as IGV; 
# a sorted-by-name SAM file is created, which is compatible with the feature-counting soft-ware of htseq-count.

for(i in seq_len(nrow(samples))) { 
  lib = samples$LibraryName[i]
  ob = file.path(lib, "accepted_hits.bam")
  
# sort by name, convert to SAM for htseq-count 
  cat(paste0("samtools sort -n ",ob," ",lib,"_sn"),"\n") 
  cat(paste0("samtools view -o ",lib,"_sn.sam ",lib,"_sn.bam"),"\n")
  
  # sort by position and index for IGV 
  cat(paste0("samtools sort ",ob," ",lib,"_s"),"\n") 
  cat(paste0("samtools index ",lib,"_s.bam"),"\n\n")
}

# Users should be conscious of the disk space that may get used in these operations. In the command above, sorted-by-name 
# SAM and BAM files (for htseq-count), as well as a sorted-by-chromosome-position BAM file (for IGV), are created for 
# each original accepted hits.bam file. User may wish to delete (some of) these intermediate files after the steps below.

# Inspect alignments with IGV. This operation is independent to R and, therefore, it must be conducted without R.


# Step: Feature counting

# From the set of mapped reads and either an annotation catalog or an assembled transcriptome, features, typically genes 
# or transcripts, are counted and assembled into a table (rows for features and columns for samples).
# The statistical methods, which are integral to the differential expression discovery task, 
# operate on a feature count table. Before the statistical modeling, further quality checks are encouraged to ensure
# that the biological question can be addressed.

# Add the names of the COUNT files to the metadata table and call HTSeq from the following R-generated Unix commands:

#The option -s signifies that the data are not from a stranded protocol (this may vary by experiment) 
#and the -a option specifies a minimum score for the alignment quality.

samples$countf = paste(samples$LibraryName, "count", sep = ".")
gf = "Drosophila_melanogaster.BDGP5.70.gtf"
cmd = paste0("htseq-count -s no -a 10 ", samples$LibraryName, "_sn.sam ", gf," > ", samples$countf)
system(cmd)

# Step: differential expression analysis

# DESeq and edgeR implement general differential analyses on the basis of the NB model.

# Using edgeR (simple design)

# Load the edgeR package and use the utility function, readDGE, to read in the COUNT files created from htseq-count:
library(edgeR)
counts = readDGE(samples$countf)$counts

# Filter weakly expressed and noninformative (e.g., non-aligned) features:

# In edgeR, it is recommended to remove features without at least 1 read per million in n of the samples, where n 
# is the size of the smallest group of replicates (here, n = 3 for the knockdown group).

noint = rownames(counts) %in% c("no_feature","ambiguous","too_low_aQual", "not_aligned","alignment_not_unique")
cpms = cpm(counts) 
keep = rowSums(cpms > 1) > = 3 & !noint
counts = counts[keep,]

# Visualize and inspect the count table

colnames(counts) = samples$shortname

head( counts[,order(samples$condition)], 5 )

# Create a DGEList object (edgeR's container for RNA-seq count data)

d = DGEList(counts = counts, group = samples$condition)

# Estimate normalization factors
d = calcNormFactors(d)

# Inspect the relationships between samples using a multidimensional scaling (MDS) plot
plotMDS(d, labels = samples$shortname, col = c("darkgreen","blue")[factor(samples$condition)])

# Estimate tagwise dispersion (simple design)
d = estimateCommonDisp(d)
d = estimateTagwiseDisp(d)

# Create a visual representation of the mean-variance relationship using the plotMeanVar and plotBCV functions
plotMeanVar(d, show.tagwise.vars = TRUE, NBline = TRUE)
plotBCV(d)

#Test for differential expression ('classic' edgeR)
de = exactTest(d, pair = c("CTL","KD"))

# Using DESeq (simple design)
library(DESeq)



