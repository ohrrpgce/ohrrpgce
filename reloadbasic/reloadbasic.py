#!/usr/bin/env python
import os
import sys
import re
from random import randint
import optparse
import pyPEG
from pyPEG import _not as NOT, _and as AND, ASTNode, ignore as IGNORE, ParseError, pointToError
from xmlast import AST2XML

reloadbasic = "".join((a,a.upper())[randint(0,1)] for a in "reloadbasic")


############################### RB PEG grammar #################################


type_attributes = 'ptr', 'integer', 'string', 'float', 'double', 'zstring', 'zstringsize', 'bool', 'flag'
boolean_attributes = 'required', 'multi', 'warn', 'ignore'
attributes = type_attributes + boolean_attributes + ('default',)

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

def identifier():           return re.compile(r'[_a-z]\w*')

def namespace():            return PLUS, (identifier, ".")

# Basically anything except a comma or (, ), {, }
def genericToken():         return re.compile(r"([-a-z0-9._+=<>\\@&#$^*+[\]:;]|/(?!'))+")

def nodeIndex():            return "[", CHECKPNT, "$", identifier, "]" 

def nodeSpec():             return (identifier, PLUS, (".", string, QUES, nodeIndex), 
                                    STAR, (".", CHECKPNT, re.compile('|'.join(attributes)),
                                           QUES, ("(", CHECKPNT, expression, ")")))

#def simpleNodeSpec():      return [nodeSpec, identifier]

# tokenLists are used where we don't really want to parse the input, only find nodeSpecs.
# Strings are still parsed, to make sure they don't confuse the parser.
def tokenList():            return STAR, [nodeSpec, string, IGNORE(r'[^\s"]+')]

def expressionList():       return QUES, (expression, STAR, (",", expression))

# expressions are more carefully parsed, in order to match parentheses and find commas
def expression():           return PLUS, [("(", CHECKPNT, expressionList, ")"), ("{", CHECKPNT, expressionList, "}"),
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


class LanguageError(ParseError):
    def __init__(self, message, node):
        self.message, self.node = message, node

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
            lineno = i + 1
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

class FileParsingIterator(object):
    """
    Generates a (lineno, line, node) triple for each source line of a .rbas file, where node is a lineGrammar AST node.
    """

    def __init__(self, filename):
        self.filename = filename
        file = open(filename, 'r')
        self.starting_in_comment = False
        self.parser = pyPEG.LineParser(skipComments = comment, packrat = True, forceKeywords = True)
        self.source = source_lines_iter(file)

    def __iter__(self):
        return self

    def next(self):
        self.lineno, self.line = self.source.next()
        try:
            parse_line = self.line.lower()
            offset = 0
            if self.starting_in_comment:
                parse_line = "/'..." + parse_line
                offset = -5
            ast, rest = self.parser.parse_line(parse_line, lineGrammar, matchAll = True, offset = offset)
            last_comment = self.parser.last_comment()
            self.starting_in_comment = False
            if last_comment:
                if last_comment[0].startswith("/'") and not last_comment[0].endswith("'/"):
                    self.starting_in_comment = True
            return self.lineno, self.line, ast[0]
        except ParseError, e:
            print "On line %d in %s:\n%s" % (self.lineno, self.filename, str(e))
            sys.exit(1)


################################# AST helpers ##################################


def get_ident(astnode):
    """Translate identifier ASTNode to string"""
    assert astnode.name == 'identifier'
    return astnode[0]

def get_string(astnode):
    """Translate string ASTNode to string"""
    assert astnode.name == 'string'
    if astnode[0][0] == '!':
        return astnode[0][2:-1]
    else:
        # FIXME: I'm too lazy to properly escape the string, since we don't need that support
        return astnode[0][1:-1]

def normalise_typed_var_list(astnode):
    """
    Normalise a dimStatement with individually typed variables or an argList (see normalise_var_declarations)
    """
    ret = []
    for node in astnode:
        assert node.name in ('typedVariableDecl', 'argumentDecl')
        varname = get_ident(node[0])
        if len(node) == 3:
            initval = node[2]
        else:
            initval = None
        ret.append((varname, node[1], initval))
    return ret

def normalise_typeless_var_list(astnode):
    """
    Normalise a dimStatement with individually untyped variables (see normalise_var_declarations)
    """
    ret = []
    for node in astnode.what[1:]:
        assert node.name == 'typelessVariableDecl'
        varname = get_ident(node[0])
        if len(node) == 2:
            initval = node[1]
        else:
            initval = None
        ret.append((varname, astnode[0], initval))
    return ret

def normalise_var_declarations(astnode):
    """
    Fed in a dimStatement AST node, returns a list of (varname, type_node, init_value_node) triples.
    type is a typename AST node, init_value_node is either None or an initialValue AST node.
    """
    if astnode[0].name == 'typename':
        return normalise_typeless_var_list(astnode)
    else:
        return normalise_typed_var_list(astnode)


################################# NodeSpecs ####################################


class NodeSpec(object):
    def __init__(self, node, cur_line, force_type = None):
        self.nodeptr = get_ident(node[0])
        self.indices = []    # string ASTNodes
        self.attributes = []
        self.default = 0
        self.type = None
        self.required = False
        self.multi = False
        self.warn = False
        self.ignore = False

        last_attribute = None
        for element in node[1:]:
            if isinstance(element, ASTNode):
                if element.name == "string":
                    self.indices.append(element)
                elif element.name == "nodeIndex":
                    raise LanguageError("Nodespec node value indices unimplemented", element)
                elif element.name == "expression":
                    if last_attribute != "default":
                        raise LanguageError("Only node 'default' attributes may take an argument", element)
                    self.default = cur_line[element.start:element.end]
                last_attribute = None
            else:
                # It's an attribute
                last_attribute = element
                self.attributes.append(element)
                if element in type_attributes:
                    if self.type != None:
                        LanguageError("Found nodespec with more than one type attribute", element)
                    self.type = element
                else:
                    setattr(self, element, True)

        if force_type:
            if self.type and self.type != force_type:
                raise LanguageError("May not specify value type of this nodespec to be %s (it must be %s)" % (self.type, force_type), node)
            self.type = force_type
        if self.type == None:
            self.type = "integer"
        if self.default == True:
            # This means a 'default' attribute was given, but not followed by an argument
            raise LanguageError('Expected argument after ".default", such as ".default(-1)"', node)
        if self.type == "flag" and self.default:
            raise LanguageError("Don't give a default value for nodespec value type 'flag'", node)
        if self.required and self.warn:
            raise LanguageError("Nodespec can't have both 'required' and 'warn'", node)
        if self.required and self.warn:
            raise LanguageError("Nodespec can't have both 'required' and 'default'", node)
        if self.ignore and len(self.attributes) > 1:
            raise LanguageError("An 'ignore' nodespec should not have any other attributes", node) 

    # For ptr, integer, string, float, double, zstring, zstringsize, bool, flag
    # These return NULL, 0, "", NULL, 0.0, 0.0, NULL, 0, NO, NO if {ptr} is NULL
    getters = (
        "{ptr}", "GetInteger({ptr})", "GetString({ptr})", "GetFloat({ptr})", "GetFloat({ptr})",
        "GetZString({ptr})", "GetZStringSize({ptr})", "(GetInteger({ptr}) <> 0)", "({ptr} <> NULL)"
        )

    def value_getter(self):
        """
        Returns the value getting expression for this type of Node, with proper default:
        A template string to be formatted where {ptr} is the Node ptr, which may appear more than once!
        This is NOT a complete expression to evaluate the nodespec.
        """
        getter = NodeSpec.getters[type_attributes.index(self.type)]
        if self.default != 0:
            getter = "IIF({ptr}, %s, %s)" % (getter, self.default)
        return getter

    def fast_value_getter(self):
        if self.type == "integer":
            return "IIF({ptr}, IIF({ptr}->nodeType = rltInt, {ptr}->num, GetInteger({ptr})), %s)" % self.default
        return self.value_getter()

    def path_string(self):
        """
        An RPath-like node path, but starting with a NodePtr variable name
        """
        return "/".join([self.nodeptr] + [get_string(bit) for bit in self.indices])


########################### RB to FB translation ###############################


whitespace = re.compile("\s*")

def indent(text, indentwith):
    return "".join(indentwith + a + "\n" for a in text.rstrip().split("\n"))

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

"""


"""



class ReloadBasicFunction(object):
    """
    A translator for a single """ + reloadbasic + """ function.
    """

    def __init__(self, outfile, filename):
        self.makename_idx = 0
        self.nodeptrs = set()    # all (user declared) variables which are NodePtrs
        self.max_temp_vars = 0   # The number of temp Node ptr variables needed
        self.used_temp_vars = 0  # The number currently in use during the line-by-line translation
        self.nodenames = {}      # All the node names used in this function, mapped to increasing integers
        #self.docptrs = {}        # Each nodeptr variable name is mapped to a docptr variable name

        self.outfile = outfile
        self.filename = filename
        self.warn_func = "debug"
        self.error_func = "debug"

    def makename(self, prefix = ''):
        self.makename_idx += 1
        return "%s_%d" % (prefix, self.makename_idx)

    def typename_is_nodeptr(self, node):
        """
        Given a typename AST node, returns whether it is a NodePtr or Node ptr.
        """
        if len(node) == 2:
            return node[0] == ASTNode('identifier', ['node']) and node[1] == 'ptr'
        if len(node) == 1:
            return node[0] == ASTNode('identifier', ['nodeptr'])
        return False

    def find_nodeptr_vars(self, varnodes):
        """
        Given a list of normalised variable declarations, return the names of the Node ptrs among them.
        """
        return [varname for (varname, typenode, initval) in varnodes if self.typename_is_nodeptr(typenode)]

    def lookup_names(self, doc, namelist):
        prefix = self.makename()
        maxlen = max(len(n) for n in namelist)
        strings = ", ".join('"' + name + '"' for name in namelist)
        return function_header_text.format(doc=doc, name=name, maxlen=maxlen, strings=strings)

    def assign_to_temp_nodeptr(self, expression):
        """
        Create a temporary Node ptr variable with some initial value (reusing variables where possible).
        Returns a (varname, init_string) pair, where init_string is a FB source statement.
        """
        name = "_node%d" % self.used_temp_vars
        self.used_temp_vars += 1
        if self.max_temp_vars < self.used_temp_vars:
            self.max_temp_vars = self.used_temp_vars
            return name, "DIM %s as NodePtr = %s\n" % (name, expression)
        return name, "%s = %s\n" % (name, expression)

    def namenum(self, parent_nodeptr, stringnode):
        """
        Returns a snippet of FB source that computes the namenum for a RELOAD node name given as a string ASTNode
        """
        stringhash = reload_HashZString(get_string(stringnode))
        return 'GetNameNumWithHash(%s, @%s, %d)' % (self.docptrs[parent_nodeptr], stringnode[0], stringhash)

        #nameindex = self.nodenames.setdefault(name, len(self.nodenames))
        #return "%s->" % (self.get_docptr(nodeptr), nameindex)
            
    def nodespec_translation(self, astnode, wantptr = False):
        """
        Given a nodeSpec ASTNode for a nodeSpec which is in normal imperative scope, return a pair of strings:
        (translation, prologue)
        where translation is FB translation to insert directly, and prologue is a set of source lines to place in front.
        wantptr is True when the result should be a Node ptr rather than the value of the node.
        """
        if wantptr:
            nodespec = NodeSpec(astnode, self.cur_line, "ptr")
        else:
            nodespec = NodeSpec(astnode, self.cur_line)
        if nodespec.nodeptr not in self.nodeptrs:
            raise LanguageError("Nodespec lead variable is not recognised as a Node ptr", astnode[0])

        nodeptr = nodespec.nodeptr
        #index_namenums = [self.namenum(nodespec.nodeptr, index) for index in nodespec.indices]
        #for namenum in index_namenums:
            #nodeptr = "GetChildByNameNum(%s, %s)" % (nodeptr, namenum)

        for namenode in nodespec.indices:
            nodeptr = 'GetChildByName(%s, "%s")' % (nodeptr, get_string(namenode))

        getter = nodespec.value_getter()

        use_temp = nodespec.required or nodespec.warn or (len(self.cur_line) + len(nodeptr) > 120) or getter.count("{ptr}") > 1
        if use_temp:
            nodeptr, prologue = self.assign_to_temp_nodeptr(nodeptr)
        else:
            prologue = ""

        if nodespec.required or nodespec.warn:
            temp = 'IF %s = NULL THEN {func} "%s: {it_is} node %s missing"' % (nodeptr, self.cur_filepos, nodespec.path_string())
            if nodespec.required:
                prologue += temp.format(func = self.error_func, it_is = "Required") + ": " + self.exit + "\n"
            if nodespec.warn:
                prologue += temp.format(func = self.warn_func, it_is = "Expected") + "\n"

        return (getter.format(ptr = nodeptr), prologue)

    def translate_nodespecs(self, nodeset):
        """
        Translate all the nodespecs within a tokenList, expression, expressionList or plain list of these, returning a (replacements, prologue) pair.
        """
        replacements = []
        prologue = ""
        for node in nodeset:
            if isinstance(node, ASTNode):
                if node.name == "nodeSpec":
                    temp = self.nodespec_translation(node)
                    replacements.append((node, temp[0]))
                    prologue += temp[1]
                else:
                    replacement, temp = self.translate_nodespecs(node)
                    replacements.extend(replacement)
                    prologue += temp
        return replacements, prologue

    def perform_replacements(self, replacements):
        """
        Return a copy of the current line, with some ASTNodes replaced with strings.
        """
        ret = ""
        start = 0
        for node, replacement in replacements:
            # print "Replacing", AST2XML(node)
            # print self.cur_line
            # print rep
            ret += self.cur_line[start:node.start] + replacement
            start = node.end
        ret += self.cur_line[start:]
        return ret

    def readnode(self, node, items):
        it = makename("ch")
        cases = (['CASE {}']
                 + 'CASE ELSE: warn_unexpected({it})')
        cases = "    \n".join(cases).format(it=it)
        return readnode_text.format(it=it, node=node, nametbl=nametbl, cases=cases)

    def output(self, text, prologue = ""):
        """Write something to the output"""
        if len(prologue):
            indentwith = whitespace.match(self.cur_line)
            if indentwith:
                prologue = indent(prologue, indentwith.group(0))
        self.outfile.write(prologue + text + "\n")

    def process_function(self, iterator, header, xml_dump = False):
        """
        header is a subStart or functionStart ASTNode.
        """
        # These are RELOAD nodes, not ASTNodes
        self.nodeptrs = []

        self.name = get_ident(header.identifier)

        args = header.get("argList")
        if args:
            self.nodeptrs = self.find_nodeptr_vars(normalise_typed_var_list(args))
        #print "Found nodeptrs in args:", local_nodeptrs

        if header.name == "subStart":
            self.exit = "EXIT SUB"
        else:
            self.exit = "RETURN 0"

        for lineno, line, node in iterator:
            if xml_dump:
                sys.stderr.write("\nline %d:\n" % lineno + AST2XML(node))

            # The root lineGrammar node has either 0 or 1 children
            if len(node) == 0:
                self.output(line)
                continue
            node = node[0]

            self.cur_filepos = "%s:%s, in %s" % (self.filename, lineno, self.name)
            self.cur_line = line
            self.used_temp_vars = 0

            nodetype = node.name
            #print nodetype
            if nodetype == "dimStatement":
                var_list = normalise_var_declarations(node)
                temp = self.find_nodeptr_vars(var_list)
                #print "Line", lineno, "Found nodeptrs:", temp
                self.nodeptrs.extend(temp)
                # Check for nodespecs in initial values
                intvals = (initval for _, _, initval in var_list if initval)
                replacements, prologue = self.translate_nodespecs(intvals)
                self.output(self.perform_replacements(replacements), prologue)
            elif nodetype == "tokenList":
                replacements, prologue = self.translate_nodespecs(node)
                #print "tokens", replacements, repr(prologue)
                self.output(self.perform_replacements(replacements), prologue)
            elif nodetype in ("subEnd", "functionEnd"):
                self.output(line)
                return
            else:
                self.output(line)


class ReloadBasicTranslator(object):
    def __init__(self):
        pass

    def process_file(self, filename, outfilename = "", xml_dump = False):
        if outfilename == "":
            outfilename = os.path.splitext(filename)[0] + ".bas"
        if outfilename == filename:
            sys.exit("Refusing to overwrite input file with output")
        outfile = open(outfilename, 'w')

        iterator = FileParsingIterator(filename)
        for lineno, line, node in iterator:
            try:
                if xml_dump:
                    sys.stderr.write("\nline %d:\n" % lineno + AST2XML(node))

                # In particular, function/sub headers are always written unmodified
                outfile.write(line + "\n")

                # The root lineGrammar node has either 0 or 1 children
                if len(node) == 0:
                    continue
                node = node[0]

                nodetype = node.name
                #print nodetype
                if nodetype in ("subStart", "functionStart"):
                    ReloadBasicFunction(outfile, filename).process_function(iterator, node, xml_dump)
                elif nodetype == "dimStatement":
                    pass
                elif nodetype != "tokenList":
                    raise ParseError("Found unexpected " + nodetype + " outside any function")

            except ParseError, e:
                if hasattr(e, "node"):
                    e.message += "\n" + pointToError(iterator.line, e.node.start, e.node.end)
                print "On line %d of %s:\n%s" % (iterator.lineno, filename, str(e))
                sys.exit(1)


################################################################################


if __name__ == "__main__":
    parser = optparse.OptionParser(usage="%prog [-o outfile.bas] infile.rbas", description="Translate a " + reloadbasic + " source file to a FreeBASIC source file.")
    parser.add_option("-o", dest="outfile", default="",
                      help="the name of the output file, defaults to <infile>.bas")
    parser.add_option("-t", "--trace", action="store_true", dest="trace", default=False,
                      help="output a parser debugging trace to stderr")
    parser.add_option("-x", "--xml", action="store_true", dest="xml", default=False,
                      help="dump AST tree of each source line to stderr as XML")

    (options, args) = parser.parse_args()
    if len(args) != 1:
        parser.print_help()
        sys.exit("Error: Expected exactly one input file.")
    pyPEG.print_trace = options.trace

    translator = ReloadBasicTranslator()
    translator.process_file(args[0], options.outfile, options.xml)
