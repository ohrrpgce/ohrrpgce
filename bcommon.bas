'OHRRPGCE - Custom+Game common battle system code
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GNU GPL details and disclaimer of liability

#include "const.bi"
#include "util.bi"
#include "config.bi"

'This is similar to fuzzythreshold. It interpolates between these values:
' 0.12  0.24  1.00  2.00 ...  x
'  0     1     0     1      x - 1 
FUNCTION fuzzy_strong_amount (byval value as double) as double
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
FUNCTION awful_compatible_equip_elemental_merging (values() as single) as single
 DIM sign as integer = 1
 'fuzzy logic! 0 <= weak_bit <= 1,  0 <= strong_amount
 'strong_amount is a fuzzy truth value for "doubling", generalised to more than 200%
 DIM as double weak_bit = -1.0, strong_amount = -1.0
 DIM as double immunemult = 1.0
 FOR i as integer = 0 TO UBOUND(values)
  IF isfinite(values(i)) = NO THEN CONTINUE FOR
  IF values(i) < 0 THEN sign = -1
  DIM absval as double = ABS(values(i))

  weak_bit = large(weak_bit, fuzzythreshold(absval, 1.0, 0.24))
  strong_amount = large(strong_amount, fuzzy_strong_amount(absval)) 
  'These fuzzythresholds simulate an 'immune' bit, restoring just a shred of sanity to the results
  immunemult *= fuzzythreshold(absval, 0, 0.12)
 NEXT

 DIM as double weakmult, strongmult
 weakmult = weak_bit * 0.12 + (1.0 - weak_bit) * 1.0
 strongmult = strong_amount * 2.0 + (1.0 - strong_amount) * 1.0
 RETURN sign * weakmult * strongmult * immunemult
END FUNCTION

'Merge elemental resist values from one item into the hero's cumulative resists,
'by multiplication (a sane version of the old way)
FUNCTION multiplicative_equip_elemental_merging (values() as single) as single
 DIM sign as integer = 1
 DIM ret as double = 1.0
 FOR i as integer = 0 TO UBOUND(values)
  IF isfinite(values(i)) = NO THEN CONTINUE FOR
  IF values(i) < 0 THEN sign = -1
  ret *= ABS(values(i))
 NEXT
 RETURN sign * ret
END FUNCTION

'Merge elemental resist values from one item into the hero's cumulative resists,
'by adding together the differences from 1.0
FUNCTION additive_equip_elemental_merging (values() as single) as single
 DIM ret as double = 1.0
 FOR i as integer = 0 TO UBOUND(values)
  IF isfinite(values(i)) = NO THEN CONTINUE FOR
  ret += values(i) - 1.0
 NEXT
 RETURN ret
END FUNCTION

FUNCTION equip_elemental_merge(values() as single, byval formula as integer) as single
 SELECT CASE formula
  CASE 0:
   RETURN awful_compatible_equip_elemental_merging(values())
  CASE 1:
   RETURN multiplicative_equip_elemental_merging(values())
  CASE 2:
   RETURN additive_equip_elemental_merging(values())
 END SELECT
END FUNCTION
