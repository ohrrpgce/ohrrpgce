'OHRRPGCE - Custom+Game common battle system code
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GNU GPL details and disclaimer of liability

#include "util.bi"
#include "compat.bi"

OPTION EXPLICIT


'This is similar to fuzzythreshold. It interpolates between these values:
' 0.12  0.24  1.00  2.00 ...  x
'  0     1     0     1      x - 1 
FUNCTION fuzzy_strong_amount (value as double) as double
 IF value <= 0.12 THEN
  RETURN 0.0
 ELSEIF value <= 0.24 THEN
  RETURN (value - 0.12) / 0.12
 ELSEIF value <= 1.0 THEN
  RETURN (1.0 - value) / 0.76
 ELSE
  RETURN value - 1.0
 END IF
END FUNCTION

'Merge elemental resist values from one item into the hero's cumulative resists,
'by simulating the old way of just ORing together all the bits. Uses linear
'interpolation to generalise the required results on inputs of combinations of
'0, 12, 24, 100, 200% to arbitrary values. The results can be pretty unexpected.
FUNCTION awful_compatible_equip_elemental_merging (byval val1 as double, byval val2 as double) as double
 DIM sign as integer = 1.0
 IF val1 < 0 OR val2 < 0 THEN sign = -1.0
 val1 = ABS(val1)
 val2 = ABS(val2)
 'fuzzy logic! also known as, in the hands of madmen, The Abyss
 DIM as double weak_bit_1, weak_bit_2
 weak_bit_1 = fuzzythreshold(val1, 1.0, 0.24)
 weak_bit_2 = fuzzythreshold(val2, 1.0, 0.24)
 DIM as double strong_bit_1, strong_bit_2
 strong_bit_1 = fuzzy_strong_amount(val1)
 strong_bit_2 = fuzzy_strong_amount(val2)
 IF weak_bit_2 > weak_bit_1 THEN weak_bit_1 = weak_bit_2  'values at most 1.0, so result tends to 0.24
 IF strong_bit_2 > strong_bit_1 THEN strong_bit_1 = strong_bit_2
 DIM as double weakmult, strongmult
 weakmult = weak_bit_1 * 0.12 + (1.0 - weak_bit_1) * 1.0
 strongmult = strong_bit_1 * 2.0 + (1.0 - strong_bit_1) * 1.0
 'These fuzzythresholds simulate an 'immune' bit, restoring just a shred of sanity to the results
 val1 = weakmult * strongmult * fuzzythreshold(val1, 0, 0.12) * fuzzythreshold(val2, 0, 0.12)
 RETURN val1 * sign
END FUNCTION

'Merge elemental resist values from one item into the hero's cumulative resists,
'by multiplication (a sane version of the old way)
FUNCTION multiplicative_equip_elemental_merging (byval val1 as double, byval val2 as double) as double
 DIM sign as integer = 1.0
 IF val1 < 0 OR val2 < 0 THEN sign = -1.0
 val1 = ABS(val1)
 val2 = ABS(val2)
 val1 *= val2
 RETURN val1 * sign
END FUNCTION

'Merge elemental resist values from one item into the hero's cumulative resists,
'by adding together the differences from 1.0
FUNCTION additive_equip_elemental_merging (byval val1 as double, byval val2 as double) as double
 RETURN (val1 - 1.0) + (val2 - 1.0) + 1.0
END FUNCTION

FUNCTION equip_elemental_merge(byval val1 as double, byval val2 as double, byval formula as integer) as double
 SELECT CASE formula
  CASE 0:
   RETURN awful_compatible_equip_elemental_merging(val1, val2)
  CASE 1:
   RETURN multiplicative_equip_elemental_merging(val1, val2)
  CASE 2:
   RETURN additive_equip_elemental_merging(val1, val2)
 END SELECT
END FUNCTION
