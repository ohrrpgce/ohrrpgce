""" Autoindent a .BAS file """
import sys

def sw(s,l):
    for j in l:
        if s.startswith(j):
            return True
    return False

tab_kw = [ "SELECT CASE","FUNCTION", "SUB", "DO", "WHILE", "FOR"]
untab_kw = [ "END SUB", "END FUNCTION", "LOOP", "WEND", "NEXT"]

# You can swap this for \t when using the simple-indenter
# beware excessive line length though
indentstr = "    "

#customize the adaptive indenter using this.
#make sure the last item uses a threshold which your code will not exceed.
adaptive_data = (
    #threshold, indentstring
#    (        0, "\t"),
#    (        2, "    "),
#     ( 99999999, "  "),
     ( 99999999, " "),
)

#the above in english:
# the first two indentation will be 4 spaces per indent
# after that, all will be 2 space indents.


def adaptive(n):
    r = ""
    for i in adaptive_data:
	take = min(i[0], n)
	n -= take
	r += i[1] * take

    # replace [eight spaces] with [tab]
    r = r.replace("        ","\t")

    return r

def simple(n):
    return indentstr * n

indent = adaptive

#delete the \x0d if you want to use unix endline format rather than dos.
endline = "\x0d\x0a"

# change in tabulation after the end of this line
latertabs = 0
tabs = 0
inselect = False

for fname in sys.argv[1:]:
    # reset state
    latertabs = 0
    tabs = 0
    inselect = False
    f = open(fname)
    o = open(fname + ".i","w")
    for i in f.readlines():
	i = i.strip()
        
	if sw(i, untab_kw):
    	    tabs -= 1
	if sw(i, tab_kw):
            latertabs += 1
	
 	# if structures
	if i.startswith("IF") and i.endswith("THEN"):
    	    latertabs += 1

	# hack!
	if i.startswith("CASE"):
	    if not inselect:
		inselect = True
		#tabs += 1
		latertabs += 1
	    else:
		latertabs += 1
		tabs -= 1
	
	if i.startswith("END SELECT"):
	    inselect = False
            tabs -= 2
	
        if i.startswith("END IF"):
            tabs -=1
        
	o.write(indent(tabs) )
	o.write(i)
	o.write(endline)
        tabs += latertabs
        latertabs = 0
    f.close()
    o.close()
