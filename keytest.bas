'$DYNAMIC
defint a-z
declare sub setkeys()
declare function Keyseg ()
declare function keyoff ()
declare function keyval (byval a)
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()

TYPE Regtype
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE
DIM regs AS Regtype

'Save old keyhandler
regs.ax = &H3509: CALL interruptx(&H21, regs, regs)
off9 = regs.bx: seg9 = regs.es

'Keyhandler On
regs.ax = &H2509: regs.ds = Keyseg: regs.dx = keyoff
CALL interruptx(&H21, regs, regs)

DIM timing(4)

'setkeys
'DO
' setwait timing(), 80
' setkeys
' for I = 1 to 90
'   if keyval(1) then print "keyval(" + ltrim$(str$(I)) + ")"
' next I
' dowait
'LOOP

'Keyhandler Off
regs.ax = &H2509: regs.ds = seg9: regs.dx = off9 
CALL interruptx(&H21, regs, regs)
