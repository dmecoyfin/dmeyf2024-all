control_leo <- c(94532075,
                 94230495,
                 93834811,
                 94207609,
                 94481778,
                 92410764,
                 95433491,
                 89757218,
                 92923891,
                 93809071,
                 96983034,
                 97458114,
                 97947873,
                 98340205,
                 97126312,
                 90555362, 
                 84577029, 
                 87639804, 
                 92805087, 
                 90160756, 
                 95654241,
                 96961565,
                 95093986,
                 96670294,
                 99206319)

control_gas <- c(100012449,
                102376872,
                102695027,
                102033326,
                100983952,
                100779119,
                100220956,
                98053219,
                98425461,
                99993059,
                97111133,
                96244910,
                97587133,
                99789989,
                101480574,
                99342401, 
                100656453, 
                101596618, 
                102647542, 
                102499765, 
                101037566,
                103855519,
                101689784,
                103164279,
                104248695)

experimental_leo <- c(104821701,
                     99320492,
                     100735783,
                     105752995,
                     83715956,
                     100964190,
                     98421270,
                     101002265,
                     100014240,
                     97719993,
                     100932640,
                     100600708,
                     102904992,
                     100983329,
                     100912367,

                     
                     99031697, 
                     99181951, 
                     99639028, 
                     100084062, 
                     100075460, 
                     95654241,
                     96961565,
                     95093986,
                     96670294,
                     99206319)             
  
experimental_gas <- c(103333920,
                     106210332,
                     101483289,
                     103223091,
                     102634882,
                     102523455,
                     102371071,
                     103394401,
                     102140239,
                     103076028,
                     98188258,
                     100232538,
                     99630773,
                     100623527,
                     99474828,
                     98593419, 
                     89966564, 
                     100570928, 
                     98015564, 
                     99163840, 
                     101453211,
                     100050542,
                     104184978,
                     102529780,
                     101649424)

length(control_leo)
length(control_gas)
length(experimental_leo)
length(experimental_gas)


control_grupoB <- c(control_leo, control_gas)
experimental_grupoB <- c(experimental_leo, experimental_gas) 

wilcox_test_leo <- wilcox.test(control_leo, experimental_leo, paired = TRUE)
print(wilcox_test_leo)

wilcox_test_gas <- wilcox.test(control_gas, experimental_gas, paired = TRUE)
print(wilcox_test_gas)

wilcox_test_grupoB <- wilcox.test(control_grupoB, experimental_grupoB, paired = TRUE)
print(wilcox_test_grupoB)

length(control_grupoB)
length(experimental_grupoB)