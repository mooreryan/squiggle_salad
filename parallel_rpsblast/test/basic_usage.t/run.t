Unzip the smp file

  $ gunzip -c PRK09103.smp.gz > PRK09103.smp

Make the target DB

  $ makeprofiledb -in rnr_target

Rpsblast can have different names

  $ RPSBLAST=`which rpsblast` || RPSBLAST=`which rpsblast+`

Run rpsblast normally for comparison

  $ OUTFMT="6 qaccver saccver pident length mismatch gapopen qstart qend sstart send evalue bitscore stitle"
  $ gunzip -c nrdA.fasta.gz | "$RPSBLAST" -query /dev/stdin -db rnr_target -outfmt "$OUTFMT" -out expected.tsv

Now run the parallel version

  $ gunzip -c nrdA.fasta.gz | parallel_rpsblast /dev/stdin rnr_target --num-threads=2 --outdir=. --rpsblast="$RPSBLAST" --force --basename=actual

Check them.  (Sort first because parallel version will change the order.

  $ sort expected.tsv > expected.sorted.tsv
  $ sort actual.tsv > actual.sorted.tsv
  $ diff expected.sorted.tsv actual.sorted.tsv
