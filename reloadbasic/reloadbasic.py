#!/usr/bin/env python
import sys
import re
import optparse
import pyPEG
from pyPEG import _not as NOT, _and as AND, Symbol, ignore as IGNORE
from xmlast import pyAST2XML


############################### RB PEG grammar #################################


attributes = 'integer', 'string', 'double', 'bool', 'flag', 'default', 'required', 'multi', 'warn', 'ignore'

# Unlike in normal grammar or regex notation, these *precede* the element they act on
CHECKPNT = -3  # no backtracking allowed once this point is reached
PLUS = -2  # +
STAR = -1  # *
QUES = 0   # ?

# Hack: since we read one line at a time, allow a /' without matching '/. Need special handling
def comment():              return re.compile(r"'[^\n]*|/'((?!'/).)*('/)?", re.S)

def basicString():          return re.compile(r'"(""|[^"\n])*"')

def escapedString():        return re.compile(r'!"(""|\\.|[^"\n])*"')

#def string():              return [escapedString, basicString]
def string():               return re.compile(r'"(""|[^"\n])*"|!"(""|\\.|[^"\n])*"')

def simpleIdentifier():     return re.compile(r'[_a-z]\w*')

def identifier():           return re.compile(r'[_a-z]\w*')

def namespace():            return PLUS, (identifier, ".")

# Basically anything except a comma or (, ), {, }
def genericToken():         return re.compile(r"([-a-z0-9._+=<>\\@&#$^*+[\]:;]|/(?!'))+")

def nodeIndex():            return "[", CHECKPNT, "$", identifier, "]" 

def nodeSpec():             return (identifier, PLUS, (".", string, QUES, nodeIndex), 
                                    STAR, (".", CHECKPNT, re.compile('|'.join(attributes))))

#def simpleNodeSpec():      return [nodeSpec, identifier]

# tokenLists are used where we don't really want to parse the input, only find nodeSpecs.
# Strings are still parsed, to make sure they don't confuse the parser.
def tokenList():            return STAR, [nodeSpec, string, IGNORE(r'[^ \n"]+')]

def expressionList():       return QUES, (expression, STAR, (",", expression))

# expressions are more carefully parsed, in order to match parentheses and find commas
def expression():           return PLUS, [("(", expressionList, ")"), ("{", expressionList, "}"),
                                          nodeSpec, string, genericToken]

def typename():             return QUES, namespace, identifier, STAR, re.compile('ptr|vector')

def arrayDimension():       return "(", CHECKPNT, expressionList, ")"

# Of a variable, or argument default
def initialValue():         return "=", CHECKPNT, expression

# Inside one style of DIM
def typedVariableDecl():    return CHECKPNT, identifier, QUES, arrayDimension, "as", typename, QUES, initialValue

# Inside the other style of DIM
def typelessVariableDecl(): return CHECKPNT, identifier, QUES, arrayDimension, QUES, initialValue

# Inside an arg list
def argumentDecl():         return QUES, ["byval", "byref"], identifier, QUES, arrayDimension, "as", typename, QUES, initialValue

def dimStatement():         return [("dim", QUES, "shared", "as", CHECKPNT, typename, typelessVariableDecl, STAR, (",", typelessVariableDecl)),
                                    ("dim", QUES, "shared", CHECKPNT, typedVariableDecl, STAR, (",", typedVariableDecl))]

def argList():              return "(", CHECKPNT, [")", (argumentDecl, STAR, (",", argumentDecl), ")")]

def functionDecorators():   return QUES, "cdecl"

def functionStart():        return QUES, "private", "function", CHECKPNT, identifier, functionDecorators, QUES, argList, "as", typename
def functionEnd():          return "end", "function"

def subStart():             return QUES, "private", "sub", CHECKPNT, identifier, functionDecorators, QUES, argList
def subEnd():               return "end", "sub"

def readnode():             return "readnode", CHECKPNT, [nodeSpec, identifier], QUES, ("as", identifier)
def readnodeEnd():          return "end", "readnode"

def withnode():             return "withnode", CHECKPNT, [nodeSpec, identifier], QUES, ("as", identifier)
def withnodeEnd():          return "end", "withnode"

def fornode():              return "fornode", CHECKPNT, string, "in", [nodeSpec, identifier], QUES, ("as", identifier)
def fornodeEnd():           return "nextnode"

def nodeSpecAssignment():   return nodeSpec, "=", CHECKPNT, expression

# Grammar for any line of RB source. Matches empty lines too (including those with comments)
# The AND element requires the regex to match before most patterns are checked.
def lineGrammar():          return [(AND(re.compile('(end\s+)?(dim|fornode|nextnode|readnode|withnode|private|sub|function)')),
                                     [dimStatement, readnode, readnodeEnd, withnode, withnodeEnd, fornode, fornodeEnd,
                                      functionStart, functionEnd, subStart, subEnd]),
                                    nodeSpecAssignment, tokenList]


################################## Parsing #####################################


def source_lines_iter(lines):
    """
    Joins together lines when _ precedes a newline.
    Generates (lineno, line) pairs, where lineno is the real line number of the first line.
    Strips newlines.
    """
    accum = ""
    lineno = None
    for i, line in enumerate(lines):
        if lineno == None:
            lineno = i
        line = line.rstrip("\n")
        accum += line
        if line.endswith("_"):
            accum = accum[:-1] + " "
            continue
        yield lineno, accum
        accum = ""
        lineno = None
    if lineno != None:
        yield lineno, accum

def parsed_file_iter(filename):
    """
    Generates a (lineno, line, node) triple for each source line of a .rbas file, where node is a lineGrammar AST node.
    """
    file = open(filename, 'r')
    starting_in_comment = False
    parser = pyPEG.LineParser(skipComments = comment, packrat = True, forceKeywords = True)
    for lineno, line in source_lines_iter(file):
        try:
            if starting_in_comment:
                line = "/'..." + line
            ast, rest = parser.parse_line(line.lower(), lineGrammar, True)
            yield lineno, line, ast[0]
            last_comment = parser.last_comment()
            starting_in_comment = False
            if last_comment:
                if last_comment[0].what.startswith("/'") and not last_comment[0].what.endswith("'/"):
                    starting_in_comment = True
        except pyPEG.ParseError, e:
            print "On line %d in %s:\n%s" % (lineno + 1, filename, str(e))
            sys.exit(1)


################################# AST helpers ##################################


def get_ident(astnode):
    assert astnode.__name__ == 'identifier'
    return astnode.what[0]

def cleanup_typed_var_list(astnode):
    """
    Clean up a dimStatement with individually typed variables or an argList.
    """
    ret = []
    for node in astnode.what:
        assert node.__name__ in ('typedVariableDecl', 'argumentDecl')
        varname = get_ident(node.what[0])
        if len(node.what) == 3:
            initval = node.what[2]
        else:
            initval = None
        ret.append((varname, node.what[1], initval))
    return ret

def cleanup_typeless_var_list(astnode):
    """
    Clean up a dimStatement with individually untyped variables.
    """
    ret = []
    for node in astnode.what[1:]:
        assert node.__name__ == 'typelessVariableDecl'
        varname = get_ident(node.what[0])
        if len(node.what) == 2:
            initval = node.what[1]
        else:
            initval = None
        ret.append((varname, astnode.what[0], initval))
    return ret

def cleanup_var_declarations(astnode):
    """
    Fed in a dimStatement or argList AST node, returns a list of (varname, type, initial_value) triples.
    type is a typename AST node, initial_value is either None or an initialValue AST node.
    """
    assert astnode.__name__ == 'dimStatement'
    if astnode.what[0].__name__ == 'typename':
        return cleanup_typeless_var_list(astnode)
    else:
        return cleanup_typed_var_list(astnode)


########################### RB to FB translation ###############################


def indent(text, indentwith):
    return "\n".join(indentwith + line for line in text)

def reload_HashZString(string):
    "Returns the HashZString hash of a string"
    # This is terrible!!
    return sum(ord(a) for a in string)


file_header_text = """\
#define RELOADINTERNAL
"""

function_header_text = """\
STATIC {tbl}_names(...) as string * {maxlen} => {{{strings}}}
STATIC {tbl}() as integer
STATIC {tbl}_cached_doc as DocPtr
STATIC {tbl}_cached_numstrings as integer  'Cache numStrings too incase more are added
IF {tbl}_cached_doc <> {doc} OR {tbl}_cached_numstrings <> {doc}->numStrings THEN
  BuildNameLookupTable({doc}, {tbl}(), {tbl}_names())
  {tbl}_cached_doc = {doc}
  {tbl}_cached_numstrings = {doc}->numStrings
END IF
"""

readnode_text = """\
IF {node}->flags AND nfNotLoaded THEN LoadNode({node})
DIM {it} as NodePtr
{it} = {node}->children
WHILE {it}
  SELECT CASE AS CONST {nametbl}({it}->namenum)
    {cases}
  END SELECT
  {it} = {it}->nextSib
WEND
"""


class ReloadBasicTranslater(object):
    def __init__(self):
        self.makename_idx = 0

    def makename(self, prefix = ''):
        self.makename_idx += 1
        return "%s_%d" % (prefix, self.makename_idx)

    def typename_is_nodeptr(self, node):
        """
        Given a typename AST node, returns whether it is a NodePtr or Node ptr.
        """
        if len(node.what) == 2:
            return node.what[0] == Symbol('identifier', 'node') and node.what[1] == 'ptr'
        if len(node.what) == 1:
            return node.what[0] == Symbol('identifier', 'nodeptr')
        return False

    def find_nodeptrs(self, node):
        parsed = cleanup_var_declarations(node)
        nodeptrs = []
        nodeptrs += [varname for (varname, typenode, initval) in parsed if self.typename_is_nodeptr(typenode)]

    def lookup_names(self, doc, namelist):
        prefix = self.makename()
        maxlen = max(len(n) for n in namelist)
        strings = ", ".join('"' + name + '"' for name in namelist)
        return function_header_text.format(doc=doc, name=name, maxlen=maxlen, strings=strings)

    def readnode(self, node, items):
        it = makename("ch")
        cases = (['CASE {}']
                 + 'CASE ELSE: warn_unexpected({it})')
        cases = "    \n".join(cases).format(it=it)
        return readnode_text.format(it=it, node=node, nametbl=nametbl, cases=cases)

    def process_file(self, filename, outfile):
        for lineno, line, node in parsed_file_iter(filename):
            #print pyAST2XML(node)
            pass


################################################################################


if __name__ == "__main__":
    parser = optparse.OptionParser(usage="%prog [-o outfile.bas] infile.rbas", description="""Translate a RELOADBasic source file to a FreeBASIC source file.""")
    parser.add_option("-o", dest="outfile", default=None,
                      help="The name of the output file, defaults to <infile>.bas")
    parser.add_option("-t", "--trace", action="store_true", dest="trace", default=False,
                      help="Output a parser debugging trace to stderr")

    (options, args) = parser.parse_args()
    if len(args) != 1:
        parser.print_help()
        sys.exit("Error: Expected exactly one input file.")
    pyPEG.print_trace = options.trace

    translator = ReloadBasicTranslater()
    translator.process_file(args[0], options.outfile)
