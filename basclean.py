""" Autoindent a .BAS file """
import sys 
import sre

def sw(s,l):
    for j in l:
        if s.startswith(j):
            return True
    return False

def uncomment(line):
    "return an uncommented version of line"
    res = sre.match("(.*)(')(.*)\Z",line)
    if res:
        res = res.groups()
        comment = res[2]
        return res[0].rstrip()
    return line

always_tab_kw = ["SELECT CASE", "FUNCTION", "SUB", "TYPE"]
tab_kw = [ "DO", "WHILE", "FOR"]
untab_kw = [ "LOOP", "WEND", "NEXT"]
always_untab_kw = [ "END SUB", "END FUNCTION", "END TYPE"]

def indent_change_ok(line):
    "Return False on cases where it looks like a line should be indented but it shouldn't"

    # ca and cb specify the causes of the non indentation
    ca = ""
    cb = ""
    for i in tab_kw:
        if line.startswith(i):
            ca = i
            break
    
    for i in untab_kw:
        if i in line:
            cb = i
            break

    if ca <> "" and cb <> "":
        return False
    return True

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

#the above commented-bits in english:
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
    linecount = 1
    caselevel = 0
    
    for i in f.readlines():
        line = i.strip()
	i = uncomment(line)

        if indent_change_ok(i):
            # simple changes
            if sw(i, untab_kw):
                tabs -= 1
            if sw(i, tab_kw):
                latertabs += 1
        
        if sw(i, always_untab_kw):
            tabs -= 1
        if sw(i, always_tab_kw):
            latertabs += 1
	
 	# if structures
	if i.startswith("IF") and i.endswith("THEN"):
            #print i
    	    latertabs += 1

        # yes, THEN can be on it's own line
        if i.startswith("THEN"):
            latertabs += 1
        
        if i.startswith("ELSE"):
            tabs -=1 
            latertabs += 1

        if i.startswith("SELECT CASE"):
            caselevel += 1

	# hack!
	if i.startswith("CASE"):
	    if inselect == caselevel - 1:
		inselect += 1
		#tabs += 1
		latertabs += 1
	    else:
		latertabs += 1
		tabs -= 1
	
	if i.startswith("END SELECT"):
	    inselect -= 1
            caselevel -= 1
            tabs -= 2

        #another hack! (why would you have multi next on one line? argh)
        if i.count("NEXT") > 1:
#            latertabs -= (i.count("NEXT"))
            tabs -= (i.count("NEXT") - 1)

        #and another! (drawing.bas:airbrush())
        if (not i.startswith("LOOP")) and i.endswith("LOOP") > 0:
            tabs -= 1
	
        if i.startswith("END IF"):
            tabs -=1
        
	o.write(indent(tabs) )
	o.write(line)
	o.write(endline)
        if tabs < 0:
            print "warning: indentation level is negative on line %d!" % (linecount)
        tabs += latertabs
        latertabs = 0
        linecount +=1

    # do some safety checking
    if tabs <> 0:
        print "warning! indentation in %s wasn't 0 when the program ended! this may mean a bug in basclean or a mistake in your code!" % (fname,)
    f.close()
    o.close()

# This handles every file correctly! finally!
# apply this quickly!

#these files are slightly crazy:
# drawing.bas (multi nexts, loops)
# game.bas (then on it's own line! i didn't even know this was valid syntax!)






