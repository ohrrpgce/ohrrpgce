""" Autoindent a .BAS file """
import sys
import re

def sw(s,l):
    for j in l:
        if s.startswith(j):
            return True
    return False

tab_kw = [ "FUNCTION", "SUB", "DO", "WHILE", "FOR"]
untab_kw = [ "END SUB", "END FUNCTION", "LOOP", "WEND"]

ignore_regex = []
for regex in ( "^DO\b.*?\bLOOP", "^WHILE\b.*?\bWEND", "^FOR\b.*?\bNEXT" ):
    ignore_regex.append(re.compile(regex))

next_regex = re.compile("^NEXT( [a-z][a-z0-9]?)?")

# You can swap this for \t when using the simple-indenter
# beware excessive line length though
indentstr = "  "

#customize the adaptive indenter using this.
#make sure the last item uses a threshold which your code will not exceed.
adaptive_data = (
    #threshold, indentstring
    (        0, "\t"),
    (        2, "    "),
    ( 99999999, "  "),
)

def adaptive(n):
    r = ""
    for i in adaptive_data:
	take = min(i[0], n)
	n -= take
	r += i[1] * take

    return r

def simple(n):
    return indentstr * n

indent = simple

#delete the \x0d to use unix endline format rather than dos.
endline = "\x0d\x0a"

# change in tabulation after the end of this line
latertabs = 0
tabs = 0


for fname in sys.argv[1:]:
    f = open(fname)
    o = open(fname + ".i","w")
    for i in f.readlines():
	i = i.strip()
        
	if sw(i, untab_kw):
    	    tabs -= 1
	if sw(i, tab_kw):
            latertabs += 1

        # support for specfic NEXT commands
        if next_regex.match(i):
            tabs -= 1
	
 	# if structures
	if i.startswith("IF") and i.endswith("THEN"):
    	    latertabs += 1

        if i.startswith("ELSE"):
            tabs -=1
    	    latertabs += 1

        if i.startswith("END IF"):
            tabs -=1
        
	o.write(indent(tabs) )
	o.write(i)
	o.write(endline)
        tabs += latertabs
        latertabs = 0
    f.close()
    o.close()
