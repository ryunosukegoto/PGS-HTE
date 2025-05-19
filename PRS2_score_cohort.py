#!/usr/bin/env python
# coding: utf-8

# Import weights and score cohort
import os
import csv
bucket = os.getenv("WORKSPACE_BUCKET")

# depression
# from https://www.pgscatalog.org/trait/MONDO_0002009/
link = "https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS000145/ScoringFiles/Harmonized/PGS000145_hmPOS_GRCh38.txt.gz"
filename = "PGS000145_hmPOS_GRCh38.txt"
output_txt = 'depression_PRS_weights.txt'

get_ipython().system('wget {link}')
get_ipython().system('gunzip -f {filename}.gz')

selected_columns = ["effect_allele", "effect_weight", "hm_chr", "hm_pos"]
 
# Read the file and select the desired columns
with open(filename, 'r', encoding='utf-8') as infile, open(output_txt, 'w', encoding='utf-8', newline='') as outfile:
    
    lines = (line for line in infile if not line.startswith('#'))
    
    reader = csv.DictReader(lines, delimiter='\t')
    writer = csv.DictWriter(outfile, fieldnames=selected_columns, delimiter='\t')
    
    # Check header and debug
    print(f"Header in the file: {reader.fieldnames}")

    # Write the header
    writer.writeheader()
    
    for row in reader:
        # Ensure all selected columns are present in the row
        if all(col in row and row[col] for col in selected_columns):
            # Select only the desired columns
            selected_row = {col: row[col] for col in selected_columns}
            writer.writerow(selected_row)


print(f"Text file '{output_txt}' saved successfully.")

get_ipython().system('gsutil cp {output_txt} {bucket}/weights/hg38/')


# self-injurious behavior
# from https://www.pgscatalog.org/trait/HP_0100716/
link = "https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS002222/ScoringFiles/Harmonized/PGS002222_hmPOS_GRCh38.txt.gz"
filename = "PGS002222_hmPOS_GRCh38.txt"
output_txt = 'sui_PRS_weights.txt'

get_ipython().system('wget {link}')
get_ipython().system('gunzip -f {filename}.gz')

selected_columns = ["effect_allele", "effect_weight", "hm_chr", "hm_pos"]
 
# Read the file and select the desired columns
with open(filename, 'r', encoding='utf-8') as infile, open(output_txt, 'w', encoding='utf-8', newline='') as outfile:
    
    lines = (line for line in infile if not line.startswith('#'))
    
    reader = csv.DictReader(lines, delimiter='\t')
    writer = csv.DictWriter(outfile, fieldnames=selected_columns, delimiter='\t')
    
    # Check header and debug
    print(f"Header in the file: {reader.fieldnames}")

    # Write the header
    writer.writeheader()
    
    for row in reader:
        # Ensure all selected columns are present in the row
        if all(col in row and row[col] for col in selected_columns):
            # Select only the desired columns
            selected_row = {col: row[col] for col in selected_columns}
            writer.writerow(selected_row)


print(f"Text file '{output_txt}' saved successfully.")

get_ipython().system('gsutil cp {output_txt} {bucket}/weights/hg38/')

# bipolar
# from https://www.pgscatalog.org/trait/MONDO_0004985/
link = "https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS002786/ScoringFiles/Harmonized/PGS002786_hmPOS_GRCh38.txt.gz"
filename = "PGS002786_hmPOS_GRCh38.txt"
output_txt = 'bipolar_PRS_weights.txt'

get_ipython().system('wget {link}')
get_ipython().system('gunzip -f {filename}.gz')

selected_columns = ["effect_allele", "effect_weight", "hm_chr", "hm_pos"]
 
# Read the file and select the desired columns
with open(filename, 'r', encoding='utf-8') as infile, open(output_txt, 'w', encoding='utf-8', newline='') as outfile:
    
    lines = (line for line in infile if not line.startswith('#'))
    
    reader = csv.DictReader(lines, delimiter='\t')
    writer = csv.DictWriter(outfile, fieldnames=selected_columns, delimiter='\t')
    
    # Check header and debug
    print(f"Header in the file: {reader.fieldnames}")

    # Write the header
    writer.writeheader()
    
    for row in reader:
        # Ensure all selected columns are present in the row
        if all(col in row and row[col] for col in selected_columns):
            # Select only the desired columns
            selected_row = {col: row[col] for col in selected_columns}
            writer.writerow(selected_row)


print(f"Text file '{output_txt}' saved successfully.")

get_ipython().system('gsutil cp {output_txt} {bucket}/weights/hg38/')


# schizophrenia
# from https://www.pgscatalog.org/trait/MONDO_0005090/
link = "https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS002785/ScoringFiles/Harmonized/PGS002785_hmPOS_GRCh38.txt.gz"
filename = "PGS002785_hmPOS_GRCh38.txt"
output_txt = 'schizophrenia_PRS_weights.txt'

get_ipython().system('wget {link}')
get_ipython().system('gunzip -f {filename}.gz')

selected_columns = ["effect_allele", "effect_weight", "hm_chr", "hm_pos"]
 
# Read the file and select the desired columns
with open(filename, 'r', encoding='utf-8') as infile, open(output_txt, 'w', encoding='utf-8', newline='') as outfile:
    
    lines = (line for line in infile if not line.startswith('#'))
    
    reader = csv.DictReader(lines, delimiter='\t')
    writer = csv.DictWriter(outfile, fieldnames=selected_columns, delimiter='\t')
    
    # Check header and debug
    print(f"Header in the file: {reader.fieldnames}")

    # Write the header
    writer.writeheader()
    
    for row in reader:
        # Ensure all selected columns are present in the row
        if all(col in row and row[col] for col in selected_columns):
            # Select only the desired columns
            selected_row = {col: row[col] for col in selected_columns}
            writer.writerow(selected_row)


print(f"Text file '{output_txt}' saved successfully.")

get_ipython().system('gsutil cp {output_txt} {bucket}/weights/hg38/')


# anxiety 
# NOTE: this PRS is for F41: other anxiety disorders:
# F41.0 Panic disorder [episodic paroxysmal anxiety]
# F41.1 Generalized anxiety disorder
# F41.2 Mixed anxiety and depressive disorder
# F41.3 Other mixed anxiety disorders
# F41.8 Other specified anxiety disorders
# F41.9 Anxiety disorder, unspecified
# from https://www.pgscatalog.org/trait/EFO_0006788/
link = "https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS004451/ScoringFiles/Harmonized/PGS004451_hmPOS_GRCh38.txt.gz"
filename = "PGS004451_hmPOS_GRCh38.txt"
output_txt = 'anxiety_PRS_weights.txt'

get_ipython().system('wget {link}')
get_ipython().system('gunzip -f {filename}.gz')

selected_columns = ["effect_allele", "effect_weight", "hm_chr", "hm_pos"]
 
# Read the file and select the desired columns
with open(filename, 'r', encoding='utf-8') as infile, open(output_txt, 'w', encoding='utf-8', newline='') as outfile:
    
    lines = (line for line in infile if not line.startswith('#'))
    
    reader = csv.DictReader(lines, delimiter='\t')
    writer = csv.DictWriter(outfile, fieldnames=selected_columns, delimiter='\t')
    
    # Check header and debug
    print(f"Header in the file: {reader.fieldnames}")

    # Write the header
    writer.writeheader()
    
    for row in reader:
        # Ensure all selected columns are present in the row
        if all(col in row and row[col] for col in selected_columns):
            # Select only the desired columns
            selected_row = {col: row[col] for col in selected_columns}
            writer.writerow(selected_row)


print(f"Text file '{output_txt}' saved successfully.")

get_ipython().system('gsutil cp {output_txt} {bucket}/weights/hg38/')


# ADHD
# from https://www.pgscatalog.org/trait/EFO_0006788/
link = "https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS002746/ScoringFiles/Harmonized/PGS002746_hmPOS_GRCh38.txt.gz"
filename = "PGS002746_hmPOS_GRCh38.txt"
output_txt = 'adhd_PRS_weights.txt'

get_ipython().system('wget {link}')
get_ipython().system('gunzip -f {filename}.gz')

selected_columns = ["effect_allele", "effect_weight", "hm_chr", "hm_pos"]
 
# Read the file and select the desired columns
with open(filename, 'r', encoding='utf-8') as infile, open(output_txt, 'w', encoding='utf-8', newline='') as outfile:
    
    lines = (line for line in infile if not line.startswith('#'))
    
    reader = csv.DictReader(lines, delimiter='\t')
    writer = csv.DictWriter(outfile, fieldnames=selected_columns, delimiter='\t')
    
    # Check header and debug
    print(f"Header in the file: {reader.fieldnames}")

    # Write the header
    writer.writeheader()
    
    for row in reader:
        # Ensure all selected columns are present in the row
        if all(col in row and row[col] for col in selected_columns):
            # Select only the desired columns
            selected_row = {col: row[col] for col in selected_columns}
            writer.writerow(selected_row)


print(f"Text file '{output_txt}' saved successfully.")

get_ipython().system('gsutil cp {output_txt} {bucket}/weights/hg38/')


import hail as hl
hl.init(default_reference='GRCh38', idempotent=True)

from bokeh.io import show, output_notebook
from bokeh.layouts import gridplot
import pandas as pd
output_notebook()
import logging

# Initialize logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# Load cohort MatrixTable

mt_path = os.environ.get("WGS_ACAF_THRESHOLD_MULTI_HAIL_PATH")

# call ACAF threshold srWGS SNPs & Indels
# https://support.researchallofus.org/hc/en-us/articles/14929793660948-Smaller-Callsets-for-Analyzing-Short-Read-WGS-SNP-Indel-Data-with-Hail-MT-VCF-and-PLINK
logger.info("Started reading the matrix table")
mt_cohort = hl.read_matrix_table(mt_path)
logger.info("Finished reading the matrix table")

sample = pd.read_csv(f'{bucket}/data/data_prs.csv')
sample['person_id'] = sample['person_id'].astype(int)
sample.head()

# filter by sample IDs of interest
selectedIndividuals = list(sample["person_id"].unique())
selectedIndividuals = [str(id) for id in selectedIndividuals]
logger.info("Started filtering columns")
mt_cohort = mt_cohort.filter_cols(hl.literal(selectedIndividuals).contains(mt_cohort.s))
logger.info("Finished filtering columns")

mt_cohort.describe()

# repartition
# mt_cohort = mt_cohort.repartition(1000)


# Score each condition

weights_files_base = ["depression_PRS_weights.txt",
                      "sui_PRS_weights.txt",
                      "anxiety_PRS_weights.txt",
                      "bipolar_PRS_weights.txt",
                      "schizophrenia_PRS_weights.txt",
                      "adhd_PRS_weights.txt",]
weights_hg38= f'{bucket}/weights/hg38'

def load_weights(path):    
    logger.info(f"Started loading weights from {path}")
    weights_ht = hl.import_table(
        path,
        types={
            "effect_allele": hl.tstr,
            "effect_weight": hl.tfloat,
            "hm_chr": hl.tstr,
            "hm_pos": hl.tint32
        }, 
        delimiter='\t'
    )
    weights_ht = weights_ht.annotate(
        hm_chr = 'chr' + weights_ht.hm_chr
    )
    weights_ht.show(5)
    weights_ht = weights_ht.annotate_globals(condition=os.path.basename(path).replace("_PRS_weights.txt",""))
    weights_ht = weights_ht.annotate(locus=hl.locus(weights_ht.hm_chr, weights_ht.hm_pos))
    weights_ht = weights_ht.key_by(weights_ht.locus)
    logger.info(f"Finished loading weights from {path}")
    return weights_ht

os.path.join(weights_hg38, "depression_PRS_weights.txt")

# check that function works
load_weights(os.path.join(weights_hg38, "depression_PRS_weights.txt"))

for weight_file in weights_files_base:
    weights = load_weights(os.path.join(weights_hg38, weight_file))
    logger.info(f"Started annotating matrix table with weights from {weight_file}")
    mt_cohort_scored = mt_cohort.annotate_rows(effect_allele = weights[mt_cohort.locus].effect_allele, 
                            effect_weight=weights[mt_cohort.locus].effect_weight)
    mt_cohort_scored = mt_cohort_scored.filter_rows(hl.is_defined(mt_cohort_scored.effect_allele))
    mt_cohort_scored = mt_cohort_scored.annotate_rows(effect_allele_index = 
                                                      mt_cohort_scored.alleles.index(
                                                          mt_cohort_scored.effect_allele)
                                                     )
    mt_cohort_scored = mt_cohort_scored.annotate_cols(score=
                                              hl.agg.sum(
                                                  hl.if_else(
                                                      hl.is_defined(mt_cohort_scored.effect_allele_index),
                                                      mt_cohort_scored.GT.
                                                      one_hot_alleles(mt_cohort_scored.alleles.length())[mt_cohort_scored.effect_allele_index],
                                                      0)
                                                  * mt_cohort_scored.effect_weight
                                              )
                                             )
    logger.info(f"Finished annotating matrix table with weights from {weight_file}")
    

    logger.info(f"Started exporting scores to CSV for {weight_file}")
    score_pd = mt_cohort_scored.cols().to_pandas()
    #out_path = os.path.join(bucket, "scores", f'{weight_file.replace("_PRS_weights.txt","")}_scores.csv')
    out_path = f'{weight_file.replace("_PRS_weights.txt","")}_scores_nostd.csv'
    score_pd.to_csv(out_path, index = False)
    logger.info(f"Finished exporting scores to CSV for {weight_file}")

# Export to cloud
# Executing this may cause a suspension from the AoU workbench. Before exporting to the cloud, make sure to send a large data request form from the following link:
# https://redcap.pmi-ops.org/surveys/?s=YRXMJFJ97J3WMWLE
# 
# If approved, AoU will send an email notifying the dates of download limit relaxation.

import pandas as pd
#export to cloud
for weight_file in weights_files_base:
    score_pd = pd.read_csv(f'{weight_file.replace("_PRS_weights.txt","")}_scores.csv')
    out_path = os.path.join(bucket, "scores", f'{weight_file.replace("_PRS_weights.txt","")}_scores_nostd.csv')
    score_pd.to_csv(out_path, index = False)

