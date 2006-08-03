'OHRRPGCE - Some utility code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' This file contains utility subs and functions which would be useful for
' any FreeBasic program. Nothing in here can depend on Allmodex, nor on any
' gfx or music backend, nor on any other part of the OHR
'
' Allmodex and backends may include this directly if needed, but other parts of
' GAME and CUSTOM do not need to include this. They can just include
' common.bi, and they will get these functions also.

DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION trimpath$ (filename$)
DECLARE FUNCTION trimextension$ (filename$)
DECLARE FUNCTION anycase$ (filename$)