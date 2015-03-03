# Genetische-Kryptoanalyse

Solve masc- and permchiffres with a genetic algorythm

__.pl__-files are configuration files for gnuplot.
__.sh__-files contain code for managing the other code.

run ```./doStatisticsMasc.sh``` and take some time.

I never tried this on a system different then mine,
so perhaps some files are missing.
If this code doesn't work for you, open an issue send me a mail.

## The Haskell files in Detail

### The Cryptography

Intersting stuff

* __NaturalLanguageModule.hs__ Tries to rate, how _natural_ a string is.
* __Reordering.hs__ a wrapper for ReorderingCrossOvers.hs and ReorderingMutations.hs, with some small additions
    * __ReorderingCrossOvers.hs__ different crossovers are described here
    * __ReorderingMutations.hs__ different mutations are described here
* __GenMasc.hs__ runs evolution for decrypting a masc-chiffre and prints the results in a log file
* __GenPerm.hs__ runs evolution for decrypting a permutation-chiffre and prints the results in a log file (to be honest, I concentrated more on masc in the last weeks)

There where simple things that had to be defined and 

* __MascModule.hs__ masc chiffres (encryption amd decryption) are defined here
* __SnModule.hs__ permutation chiffres (encryption amd decryption) are defined here
* __TypeModule.hs__ exports importent Types that are used in the other Haskell files.
* __BlindtextModule.hs__ Exports the encrypted text (and some more information about it).
* __NormalizeLanguageModule.hs__ Normalizes a string, so it holds no lowercase letters, spaces etc


### performace testing

I didn't optimize very much of my code, but this was interesting.

* __lookuptest.hs__ analyses the performance of differnt methods of looking up values in a list

### preparing data

These files are just for preparing data that is used elsewhere. They are not interesting, but had to be writen.

* __NaturalismHistogram.hs__ generates data for a histogram
* __ShowRandModule.hs__ lets you print data in the Rand-Monade (which is nontrivial in Haskell)
* __convertforBlindTextMonogramAnalysis.hs__ analyzing logfiles
* __convertforMultiplot.hs__ analyzing logfiles
* __convertforMonogramAnalysis.hs__ analyzing logfiles
* __convertforEliteplot.hs__ analyzing logfiles


