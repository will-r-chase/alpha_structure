library(tidyverse)
library(Rpdb)

pdb <- read.pdb("AtExpA4_hybridloop_fixed_manual_headerfix.pdb")
atoms <- pdb$atoms
atoms$chainid <- "A"
pdb$atoms <- atoms
write.pdb(x = pdb, file = "test_out.pdb")
#pdb <- read_fwf("AtExpA4_hybridloop_fixed_manual_headerfix.pdb", skip = 24, fwf_widths(c(4, 7, 2, 1, 1, 2, 3, 6, 12, 8, 8, 6, 6, 12)))

sequence <- atoms %>%
  select(recname, resname, resid) %>%
  filter(str_detect(.$recname, "ATOM")) %>%
  distinct(resname, resid)

roundUp <- function(number, divisor){
  divisor*ceiling(number / divisor)
} 

get_NAs <- function(seq_length){
  upper_lim <- roundUp(seq_length, 13)
  upper_lim - seq_length
}

seq_vec <- sequence$resname

NA_vec <- rep(NA, get_NAs(length(seq_vec)))

seq_vec <- c(seq_vec, NA_vec)

SEQRES <- as.tibble(matrix(seq_vec, ncol = 13, byrow = TRUE)) 

SEQRES <- SEQRES %>%
  mutate(label = "SEQRES", res_total = 237, chain = "A", line = 1:nrow(SEQRES)) %>%
  select(label, line, chain, res_total, everything()) %>%
  as.data.frame(., stringsAsFactors = FALSE)

SEQRES$V1 <- paste0(" ", SEQRES$V1)

column_widths <- c(6, 3, 1, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)

gdata::write.fwf(SEQRES, file = "SEQRES.txt", width = column_widths, colnames = FALSE)  

