Good stuff

  $ link_fasta_and_gff seqs.fasta seqs.gff > seqs_linked.gff
  $ diff seqs_linked.gff expected_seqs_linked.gff

Fasta seqs not in gff

  $ link_fasta_and_gff not_in_gff.fasta seqs.gff
  ##sequence-region A0A829L0B0 1 104
  ##sequence-region E1IJ12 1 104
  ##sequence-region E1M6N6 1 560
  ERROR: 'E1M6N6' is present in the GFF file but not the FASTA file
  [1]

Bad fasta header

  $ link_fasta_and_gff bad_fasta_header.fasta seqs.gff
  ERROR: Bad sequence ID in FASTA file: 's1'
  [1]

Bad files

  $ link_fasta_and_gff hthtth eneneneen
  link_fasta_and_gff: FASTA argument: no 'hthtth' file
  Usage: link_fasta_and_gff [OPTION]… FASTA GFF3
  Try 'link_fasta_and_gff --help' for more information.
  [1]
  $ link_fasta_and_gff hthtth seqs.gff
  link_fasta_and_gff: FASTA argument: no 'hthtth' file
  Usage: link_fasta_and_gff [OPTION]… FASTA GFF3
  Try 'link_fasta_and_gff --help' for more information.
  [1]
  $ link_fasta_and_gff seqs.fasta eneneneen
  link_fasta_and_gff: GFF3 argument: no 'eneneneen' file
  Usage: link_fasta_and_gff [OPTION]… FASTA GFF3
  Try 'link_fasta_and_gff --help' for more information.
  [1]
