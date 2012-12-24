#!/usr/bin/env python
import os
import sys
import re
from copy import copy
from random import randint
import optparse
import pyPEG
from pyPEG import _not as NOT, _and as AND, ASTNode, ignore as IGNORE, ParseError
from xmlast import AST2XML

reloadbasic = "".join((a,a.upper())[randint(0,1)] for a in "reloadbasic")


############################### RB PEG grammar #################################


type_attributes = 'ptr', 'integer', 'string', 'float', 'double', 'zstring', 'zstringsize', 'bool', 'exists'
boolean_attributes = 'required', 'warn', 'ignore', 'oob_error'
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

def identifier():           return re.compile(r'[_a-zA-Z]\w*')

def namespace():            return PLUS, (identifier, ".")

# Basically anything except a comma or (, ), {, }
# Operators only match a single character to ensure they separate rather than greedily gobbling
#def genericToken():         return re.compile(r"([-a-zA-Z0-9._+=<>\\@&#$^*+[\]:;]|/(?!'))+")
def genericToken():         return re.compile(r"[a-zA-Z0-9._]+|[-+=<>\\@&#$^*+[\]:;]|/(?!')")

def nodeIndex():            return "[", CHECKPNT, "$", identifier, "]" 

def nodeSpec():             return (identifier, PLUS, (".", string, QUES, nodeIndex), 
#                                    STAR, (".", CHECKPNT, re.compile('|'.join(attributes)),
                                    STAR, (".", CHECKPNT, re.compile('\w+'),
                                           QUES, ("(", CHECKPNT, expression, ")")))

#def simpleNodeSpec():      return [nodeSpec, identifier]

# tokenLists are used where we don't really want to parse the input, only find nodeSpecs.
# Strings are still parsed, to make sure they don't confuse the parser.
def tokenList():            return STAR, [nodeSpec, string, IGNORE(r'[a-zA-Z0-9._]+|[^\s"]')]

def expressionList():       return QUES, (expression, STAR, (",", expression))

# expressions are more carefully parsed, in order to match parentheses and find commas
def expression():           return PLUS, [("(", CHECKPNT, expressionList, ")"),
                                          ("{", CHECKPNT, expressionList, "}"),
                                          nodeSpec, string, genericToken]

def typename():             return STAR, namespace, identifier, STAR, re.compile('ptr|vector', re.I)

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

def functionStart():        return QUES, "private", [("function", CHECKPNT, identifier, functionDecorators, QUES, argList, "as", typename),
                                                     ("starttest", CHECKPNT, "(", identifier, ")")]
def functionEnd():          return [("end", "function"), "endtest"]

def subStart():             return QUES, "private", "sub", CHECKPNT, identifier, functionDecorators, QUES, argList
def subEnd():               return "end", "sub"

def readNode():             return "readnode", CHECKPNT, [(nodeSpec, "as", identifier), identifier], STAR, (",", re.compile("default|ignoreall"))
def readNodeEnd():          return "end", "readnode"

def withNode():             return "withnode", CHECKPNT, nodeSpec, "as", identifier
def withNodeEnd():          return "end", "withnode"

def arrayName():            return STAR, (identifier, "."), identifier

def loadArray():            return "loadarray", CHECKPNT, arrayName, "(", "$", identifier, ")", "=", expression

def nodeSpecAssignment():   return nodeSpec, "=", CHECKPNT, expression

def directive():            return "#", re.compile("warn_func|error_func"), CHECKPNT, "=", identifier

# Grammar for any line of RB source. Matches empty lines too (including those with comments)
# The AND element requires the regex to match before most patterns are checked.
def lineGrammar():          return [(AND(re.compile('(end\s+)?(dim|readnode|withnode|loadarray|private|sub|function|starttest|endtest|#)', re.I)),
                                     [dimStatement, readNode, readNodeEnd, withNode, withNodeEnd,
                                      functionStart, functionEnd, subStart, subEnd, loadArray, directive]),
                                    nodeSpecAssignment, tokenList]


################################## Parsing #####################################


class LanguageError(ParseError):
    def __init__(self, message, node):
        self.message, self.node = message, node
        #1/0


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
        if line.endswith("_") and line[-2] != "_":
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
        self.parser = pyPEG.LineParser(skipComments = comment, packrat = True, forceKeywords = True, caseInsensitive = True)
        self.source = source_lines_iter(file)

    def __iter__(self):
        return self

    def next(self):
        self.lineno, self.line = self.source.next()
        try:
            parse_line = self.line
            offset = 0
            if self.starting_in_comment:
                parse_line = "/'..." + parse_line
                offset = -5
            ast, rest = self.parser.parse_line(parse_line, lineGrammar, matchAll = True, offset = offset)
            self.ast = ast[0]  # This is a lineGrammar ASTNode
            last_comment = self.parser.last_comment()
            self.starting_in_comment = False
            if last_comment:
                if last_comment[0].startswith("/'") and not last_comment[0].endswith("'/"):
                    self.starting_in_comment = True
            return self.lineno, self.line, self.ast
        except ParseError, e:
            print "On line %d in %s:\n%s" % (self.lineno, self.filename, str(e))
            sys.exit(1)

    def line_is_blank(self):
        # The root lineGrammar node always has 1 child, since tokenList matches blank lines.
        # Actually, it is a tokenList with length 0 iff the line is composed entirely of
        # whitespace and comments.
        return self.ast[0].name == "tokenList" and self.ast[0].start == self.ast[0].end


class TranslationIteratorWrapper(FileParsingIterator):
    def __init__(self, filename, xml_dump = False):
        self.xml_dump = xml_dump
        self.hook = None
        FileParsingIterator.__init__(self, filename)

    def next(self):
        while 1:
            lineno, line, node = FileParsingIterator.next(self)
            if self.hook:
                self.hook.cur_filepos = "%s:%s, in %s" % (self.hook.filename, lineno, self.hook.name)
                self.hook.cur_line = line

            if self.xml_dump:
                sys.stderr.write("\nline %d:\n" % lineno + AST2XML(node))

            # lineGrammar always has exactly one child; return that
            return lineno, line, node[0]


################################# AST helpers ##################################


def get_ident(astnode):
    """Translate identifier ASTNode to string"""
    assert astnode.name == 'identifier'
    return astnode[0].lower()

def get_ident_with_case(astnode):
    """Translate identifier ASTNode to string, preserving case"""
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
        # Three different ways of indexing an ASTNode...
        varname = get_ident(node[0])
        initval = node.get('initialValue', None)
        ret.append((varname, node.typename, initval))
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


############################ Delayed file writer ###############################


class FileMarker(object):
    """
    A position inside a DelayedFileWriter.
    """

    def __init__(self, parent, position):
        self.parent = parent
        self.mark = position
        self.children_this_line = 0

    def write(self, line):
        self.mark[-1] += 1
        self.parent.lines.append((list(self.mark), line))
        self.children_this_line = 0

    def get_mark(self):
        """
        Get a new FileMarker that will write lines inbetween the last line written using
        this FileMarker, and the next line that will be written using it.
        """
        self.children_this_line += 1
        return FileMarker(self.parent, self.mark + [self.children_this_line, 0])


class DelayedFileWriter(FileMarker):
    """
    A wrapper around a file, allowing inserting lines later between already written lines.
    """

    def __init__(self, file):
        self.file = file
        self.lines = []
        self.parent = self
        self.mark = [0]

    def flush(self):
        self.lines.sort()
        for index, line in self.lines:
            self.file.write(line)

    def __del__(self):
        self.flush()


################################# NodeSpecs ####################################


def one_of(collection):
    if len(collection) == 1:
        return str(collection[0])
    return "one of " + ", ".join(str(item) for item in collection)

class NodeSpec(object):
    def __init__(self, node, cur_line, force_type = None, index_vars = ()):
        """
        node should be a nodeSpec ASTNode.
        force_type forces the value type to something; it can't be overridden.
        index_vars is a list of variable names which may be used for array indexing,
        by default indices are not allowed.
        """
        assert node.name == "nodeSpec"

        self.node = node
        self.root_var = get_ident(node[0])
        self.indices = []    # either 'string' or 'identifier' (index variables) ASTNodes. Might be length 0!
        self._index_var_index = None  # The index in self.indices of the index variable if any
        self.attributes = []
        self.default = None
        self.type = None
        self.required = False
        self.warn = False
        self.ignore = False
        self.oob_error = False

        last_attribute = None
        for element in node[1:]:
            if isinstance(element, ASTNode):
                if element.name == "string":
                    self.indices.append(element)
                elif element.name == "nodeIndex":
                    if len(index_vars) == 0:
                        raise LanguageError("Only nodespecs on LoadArray lines may have value indices", element)
                    if self._index_var_index == 1:
                        raise LanguageError("Only a single value index is allowed in each LoadArray nodespec", element)
                    index_var = get_ident(element.identifier)
                    if index_var not in index_vars:
                        raise LanguageError("This value index variable must be " + one_of(index_vars), element.identifier)
                    self._index_var_index = len(self.indices)
                    self.indices.append(element.identifier)
                elif element.name == "expression":
                    if last_attribute != "default":
                        raise LanguageError("Only node 'default' attributes may take an argument", element)
                    self.default = cur_line[element.start:element.end]
                last_attribute = None

            else:
                # It's an attribute
                if element.lower() not in attributes:
                    raise LanguageError("Invalid nodespec attribute '" + element + "'", node) 
                last_attribute = element
                self.attributes.append(element)
                if element in type_attributes:
                    if self.type != None:
                        raise LanguageError("Found nodespec with more than one type attribute", node)
                    self.type = element
                else:
                    setattr(self, element, True)

        if force_type:
            if self.type and self.type != force_type:
                raise LanguageError("May not specify value type of this nodespec to be %s (it must be %s)" % (self.type, force_type), node)
            self.type = force_type
        if self.default == True:
            # This means a 'default' attribute was given, but not followed by an argument
            raise LanguageError('Expected argument after ".default", such as ".default(-1)"', node)
        if self.type == "exists" and self.default != None:
            raise LanguageError("Don't give a default value for nodespec value type 'exists'", node)
        if self.required and self.warn:
            raise LanguageError("Nodespec can't have both 'required' and 'warn'", node)
        if self.required and self.warn:
            raise LanguageError("Nodespec can't have both 'required' and 'default'", node)
        if self.ignore and len(self.attributes) > 1:
            raise LanguageError("An 'ignore' nodespec should not have any other attributes", node)

    def check_expression_usage(self):
        """
        Check supplied attributes are valid when used as an expression in imperative code
        """
        if self.ignore:
            raise LanguageError("'ignore' attribute can only be used inside a ReadNode block", self.node)
        if self.oob_error:
            raise LanguageError("'oob_error' attribute can only be used in a LoadArray statement", self.node)
        if self.type == None:
            self.type = "integer"

    def check_readnode_expression_usage(self):
        """
        Check supplied attributes are valid when used inside a ReadNode block, excluding LoadArray lines
        """
        if self.type == None:
            self.type = "integer"
        if self.oob_error:
            raise LanguageError("'oob_error' attribute can only be used in a LoadArray statement", self.node)

    def check_loadarray_usage(self):
        """
        Check supplied attributes are valid when used inside a LoadArray lines
        """
        if self.type == None:
            self.type = "integer"
        if self.warn or self.required:
            raise LanguageError("'warn' and 'required' attributes are not allowed on a LoadArray nodespec", self.node)
        if self._index_var_index == None:  # > 1 is checked in __init__
            raise LanguageError("Each LoadArray nodespec needs a single [$varname] 'value index'", self.node)
        # Check the value index is on the child
        if self._index_var_index != 1:
            raise LanguageError("The [$varname] 'value index' should be after the first child name", self.node)

    def check_header_usage(self, name):
        """
        Check supplied attributes are valid when used in the header of a ReadNode or WithNode block
        nested inside a ReadNode.
        """
        if self.ignore:
            raise LanguageError("'ignore' attribute can only be used as a directive inside a ReadNode block", self.node)
        if self.oob_error:
            raise LanguageError("'oob_error' attribute can only be used in a LoadArray statement", self.node)
        # Using force_type instead
        #if self.type != None:
        #    raise LanguageError("May not specify a type attribute on a " + name + " nodespec", self.node)
        if self.default != None:
            raise LanguageError("Don't give a default value for a " + name + " nodespec", self.node)

    # For ptr, integer, string, float, double, zstring, zstringsize, bool, exists
    # GetInteger actually returns longint, which FB doesn't like inside an IIF
    # These return NULL, 0, "", NULL, 0.0, 0.0, NULL, 0, NO, NO if {ptr} is NULL
    getters = (
        "{ptr}", "CINT(GetInteger({ptr}))", "GetString({ptr})", "GetFloat({ptr})", "GetFloat({ptr})",
        "GetZString({ptr})", "GetZStringSize({ptr})", "(GetInteger({ptr}) <> 0)", "({ptr} <> NULL)"
        )

    defaults = ("NULL", "0", '""', "0.0", "0.0", "NULL", "0", "NO", "NO")

    def get_default(self):
        """
        Return an explicit default value as a string.
        """
        if self.default != None:
            return self.default
        return NodeSpec.defaults[type_attributes.index(self.type)]

    def value_getter(self):
        """
        Returns the value getting expression for this type of Node, with proper default:
        A template string to be formatted where {ptr} is the Node ptr, which may appear more than once!
        This is NOT a complete expression to evaluate the nodespec.
        """
        getter = NodeSpec.getters[type_attributes.index(self.type)]
        if self.default not in (None, '0', '0.0', '""'):
            if self.type == "string":
                # UGH
                return "iif_string(CINT({ptr}), %s, %s)" % (getter, self.default)
            else:
                return "IIF({ptr}, %s, %s)" % (getter, self.default)
        return getter

    def fast_value_getter(self):
        if self.type == "integer":
            default = self.default
            if default == None:
                default = "0"
            return "IIF({ptr}, IIF({ptr}->nodeType = rltInt, {ptr}->num, GetInteger({ptr})), %s)" % default
        return self.value_getter()

    def value_setter(self):
        if self.type == "ptr":
            raise LanguageError("Can't assign to a ptr-type nodespec", self.node)
        return "SetContent({ptr}, {value})"

    def path_string(self):
        """
        An RPath-like node path, but starting with a NodePtr variable name
        """
        # Ignore .name == "identifier" indices (value index variables)
        return self.root_var + ":/" + "/".join(get_string(bit) for bit in self.indices if bit.name == "string")


########################### RB to FB translation ###############################


whitespace = re.compile("\s*")

def indent(text, indentwith):
    """
    Indent one or more lines; text is either a string or a list of strings terminated with newlines.
    """
    lines = []
    if isinstance(text, str):
        text = [text]
    for item in text:
        bits = item.split("\n")
        if bits[-1] == "":
            del bits[-1]
        lines.extend(bits)
    return "".join(indentwith + l + "\n" for l in lines)

def reload_HashZString(string):
    """Returns the HashZString hash of a string"""
    ret = 0
    if len(string) % 2:
        string += "\0"
    for i in range(0, len(string), 2):
        ret += (ret << 15) + ord(string[i]) + (ord(string[i+1]) << 8)
    return 0xffffffff & ret


READNODE_TEMPLATE = """\
DIM {it} as NodePtr
IF {node} THEN
{buildtable}  IF {node}->flags AND nfNotLoaded THEN LoadNode({node})
  {it} = {node}->children
END IF
WHILE {it}
  DIM {nameindex} as integer = ANY
  IF {it}->namenum < {it}->doc->nameIndexTableLen THEN {nameindex} = {nametbl}[{it}->namenum] ELSE {nameindex} = 999999
  SELECT CASE AS CONST {nameindex}
{cases}
  END SELECT
  {it} = {it}->nextSib
WEND
{checks}
"""

READNODE_DEFAULTS_TEMPLATE = """\
DIM {it} as NodePtr = NULL
IF {node} THEN
{buildtable}  IF {node}->flags AND nfNotLoaded THEN LoadNode({node})
  {it} = {node}->children
END IF
DIM {nameindex} as integer = INVALID_INDEX
IF {it} THEN
 IF {it}->namenum < {it}->doc->nameIndexTableLen THEN {nameindex} = {nametbl}[{it}->namenum] ELSE {nameindex} = 999999
END IF
DO
  SELECT CASE AS CONST {nameindex}
{cases}
  END SELECT
  IF {it} THEN
    {it} = {it}->nextSib
    IF {it} THEN
      IF {it}->namenum < {it}->doc->nameIndexTableLen THEN {nameindex} = {nametbl}[{it}->namenum] ELSE {nameindex} = 999999
      CONTINUE DO
    END IF
  END IF
{checks}
  EXIT DO
LOOP
"""

# Translation for a LoadArray inside a ReadNode
READNODE_LOADARRAY_TEMPLATE = """\
{tmpi} = GetInteger({node})
IF {tmpi} >= LBOUND({array}) AND {tmpi} <= UBOUND({array}) THEN
  {array}({tmpi}) = {value}
ELSE
  {warn_func} "{codeloc}: Node {nodepath} value " & {tmpi} & " out of bounds; range " & LBOUND({array}) & " to " & UBOUND({array}){error_extra}
END IF
"""

# Appears before a ReadNode block translation, for each LoadArray line.
# Use this instead of flusharray() so that arbitrary types and expressions are supported. Also faster.
FLUSH_ARRAY_TEMPLATE = """\
' Flush {array}
FOR {tmpi} as integer = LBOUND({array}) TO UBOUND({array})
  {array}({tmpi}) = {value}
NEXT
"""

class ReloadBasicFunction(object):
    """
    Translator for a single """ + reloadbasic + """ function.
    """

    def __init__(self, outfile, filename, function_num, global_scope, be_careful, warn_func, error_func):
        self.makename_indices = {}
        self.nodeptrs = []       # all variables which are NodePtrs
        #self.max_temp_vars = 0   # The number of temp Node ptr variables needed
        #self.used_temp_vars = 0  # The number currently in use during the line-by-line translation
        self.users_temp_vars = set()  # "... AS var" variables. Declare each exactly once
        #self.docptrs = {}        # Each nodeptr variable name is mapped to a docptr variable name
        self.nodenames = set()    # All node names used in this function

        self.nameindex_tables = {}  # Maps NodePtr variables which are known to belong to a RELOAD document
                                    # with a complete nameindex table for this function to a nameindex table variable.
        self.derived_relation = {}  # Maps NodePtr variables to other NodePtr variables they're derived from

        self.function_num = function_num
        self.outfile = outfile
        self.filename = filename
        self.global_scope = global_scope
        self.be_careful = be_careful

        self.warn_func = warn_func
        self.error_func = error_func

    def makename(self, prefix = '_'):
        self.makename_indices.setdefault(prefix, 0)
        self.makename_indices[prefix] += 1
        return "%s%d" % (prefix, self.makename_indices[prefix])

    def typename_is_nodeptr(self, node):
        """
        Given a typename AST node, returns whether it is a NodePtr or Node ptr.
        """
        # Just ignore any namespaces
        while node[0].name == 'namespace':
            node = node[1:]
        if len(node) == 2:
            return get_ident(node[0]) == 'node' and node[1].lower() == 'ptr'
        if len(node) == 1:
            return get_ident(node[0]) == 'nodeptr'
        return False

    def find_nodeptr_vars(self, varnodes):
        """
        Given a list of normalised variable declarations, filter the NodePtrs (dropping type).
        """
        return [(varname.lower(), initval) for (varname, typenode, initval) in varnodes if self.typename_is_nodeptr(typenode)]

    def assign_to_temp_var(self, prefix, type, expression):
        """
        Returns a (varname, init_string) pair, where init_string is a FB source statement.
        """
        name = self.makename(prefix)
        if type == "NodePtr":
            self.nodeptrs.append(name)
        return name, "DIM %s as %s = %s\n" % (name, type, expression)

    def set_derived_from(self, variable, derived_from):
        """
        Mark one NodePtr variable as belonging to the same document as another.
        """
        # Don't allow a tree of depth more than 1, because one of the intermediates
        # could be an "AS foo" temporary variable which switches between multiple
        # documents. Of course, the user could do the same with their own temp
        # variables, which we don't defend against.
        while derived_from in self.derived_relation:
            derived_from = self.derived_relation[derived_from]
        self.derived_relation[variable] = derived_from

    def assign_to_temp_nodeptr(self, expression, derived_from = None):
        """
        Create a temporary Node ptr variable with some initial value <s>(reusing variables where possible)</s>.
        derived_from is the name of another nodeptr variable which is known to belong to the same RELOAD document.
        Returns a (varname, init_string) pair, where init_string is a FB source statement.
        """
        variable, statement = self.assign_to_temp_var("_node", "NodePtr", expression)
        if derived_from:
            self.set_derived_from(variable, derived_from)
        return variable, statement

        # name = "_node%d" % self.used_temp_vars
        # self.used_temp_vars += 1
        # if self.max_temp_vars < self.used_temp_vars:
        #     self.max_temp_vars = self.used_temp_vars
        #     return name, "DIM %s as NodePtr = %s\n" % (name, expression)
        # return name, "%s = %s\n" % (name, expression)

    def intern_nodename(self, name):
        self.nodenames.add(name)
        return self.global_scope.nameindex(name)

    def ensure_nameindex_table(self, nodeptr, node_not_null):
        """
        Ensure that a given NodePtr variable belongs to a RELOAD document with a namenum->nameindex
        table that has a superset of all the node names used in this function.
        node_not_null is true if the nodeptr is known to not be null.
        """
        if not self.be_careful:
            if nodeptr in self.derived_relation:
                nodeptr = self.derived_relation[nodeptr]
                # set_derived_from should not allow this
                assert nodeptr not in self.derived_relation

        if nodeptr not in self.nameindex_tables:
            buildtable = "BuildNameIndexTable(%s->doc, _nodenames(), %s, RB_FUNC_BITS_ARRAY_SZ, RB_SIGNATURE, RB_NUM_NAMES)\n" % (nodeptr, self.function_num)
            if not node_not_null:
                buildtable = "IF %s THEN %s" % (nodeptr, buildtable)
            self.nameindex_tables[nodeptr] = None
            return nodeptr, buildtable
        return nodeptr, ""

    def nameindex_table(self, nodeptr):
        """
        Return expressions for getting the nameindex table for a NodePtr. (See also ensure_nameindex_table)
        Returns (nametable_var, prologue1, prologue2)
        prologue1 is normal, prologue2 should be protected with a  nodeptr <> NULL guard.
        """
        nodeptr, buildtable = self.ensure_nameindex_table(nodeptr, True)

        if self.nameindex_tables[nodeptr] != None:
            return self.nameindex_tables[nodeptr], "", ""
        else:
            nametable = self.makename("_table")
            prologue1 = "DIM %s as short ptr\n" % nametable
            prologue2 = buildtable + "%s = %s->doc->nameIndexTable" % (nametable, nodeptr)
            
            #nametable, assignment = self.assign_to_temp_var("_table", "short ptr", nodeptr + "->doc->nameIndexTable")
            self.nameindex_tables[nodeptr] = nametable
            #return nametable, buildtable + assignment
            return nametable, prologue1, prologue2

    def get_descendant(self, nodespec):
        """
        A FB expression for finding a descendant Node described by a nodespec.
        """
        prologue = ""
        nodeptr = nodespec.root_var # "{ptr}"
        if self.be_careful:
            for namenode in nodespec.indices:
                nodeptr = 'GetChildByName(%s, "%s")' % (nodeptr, get_string(namenode))
        else:
            note = []
            
            _, prologue = self.ensure_nameindex_table(nodespec.root_var, False)
            for namenode in nodespec.indices:
                nodeptr = 'GetChildByNameIndex(%s, %s)' % (nodeptr, self.intern_nodename(get_string(namenode)))
                note.append(get_string(namenode))
            if len(note):
                nodeptr += " /'%s'/" % ".".join(note)
        return nodeptr, prologue

    def simple_nodespec_translation(self, nodespec, use_temp = False):
        """
        Like nodespec_translation without .warn/.required checking.
        """
        if nodespec.root_var.lower() not in self.nodeptrs:
            raise LanguageError("Nodespec lead variable is not recognised as a Node ptr", nodespec.node[0])

        nodeptr, prologue = self.get_descendant(nodespec)

        getter = nodespec.value_getter()

        if (len(nodespec.indices) > 0
            and ((len(self.cur_line) + len(nodeptr) > 120) or getter.count("{ptr}") > 1)):
            use_temp = True

        if use_temp:
            nodeptr, prologue_ = self.assign_to_temp_nodeptr(nodeptr, nodespec.root_var)
            prologue += prologue_

        return nodeptr, getter.format(ptr = nodeptr), prologue

    def block_nodespec_translation(self, nodespec, resultptr):
        """
        Translate a nodespec (type ptr) and put the result in the variable resultptr.
        For WithNode and ReadNode header nodespecs.
        """
        if nodespec.root_var.lower() not in self.nodeptrs:
            raise LanguageError("Nodespec lead variable is not recognised as a Node ptr", nodespec.node[0])

        # resultptr is a temp variable possible used in multiple blocks, but this should be safe
        self.set_derived_from(resultptr, nodespec.root_var)
        self.nodeptrs.append(resultptr)

        resultptr_expression, prologue = self.get_descendant(nodespec)

        if resultptr in self.users_temp_vars:
            prologue += "%s = %s\n" % (resultptr, resultptr_expression)
        else:
            prologue += "DIM %s as NodePtr = %s\n" % (resultptr, resultptr_expression)
            self.users_temp_vars.add(resultptr)

        prologue += self.nodespec_warn_required_checks(nodespec, resultptr)
        return prologue

    def nodespec_warn_required_checks(self, nodespec, nodeptr):
        """
        Returns a prologue doing .warn & .required checks for a nodespec (result in nodeptr variable)
        """
        if nodespec.required or nodespec.warn:
            temp = 'IF %s = NULL THEN {func} "%s: {it_is} node %s missing"' % (nodeptr, self.cur_filepos, nodespec.path_string())
            if nodespec.required:
                return temp.format(func = self.error_func, it_is = "Required") + ": " + self.exit + "\n"
            if nodespec.warn:
                return temp.format(func = self.warn_func, it_is = "Expected") + "\n"
        return ""

    def nodespec_translation(self, nodespec):
        """
        Given a nodeSpec ASTNode for a nodeSpec which is in normal imperative scope, return a pair of strings:
        (translation, prologue)
        where translation is FB translation to insert directly, and prologue is a list of source lines to place in front.
        """
        nodeptr, translation, prologue = self.simple_nodespec_translation(nodespec, nodespec.required or nodespec.warn)
        prologue += self.nodespec_warn_required_checks(nodespec, nodeptr)

        return translation, prologue

    def find_nodespecs(self, nodeset, results = None):
        """
        Return a list of all the nodeSpec ASTNodes (in order) within a tokenList, expression, expressionList or list of these.

        """
        if results == None:
            results = []
        for node in nodeset:
            if isinstance(node, ASTNode):
                if node.name == "nodeSpec":
                    results.append(node)
                else:
                    self.find_nodespecs(node, results)
        return results

    def translate_nodespecs(self, nodeset):
        """
        Translate all the nodespecs within a tokenList, expression, expressionList or plain list of these, returning a (replacements, prologue) pair.
        """
        replacements = []
        prologue = ""
        for astnode in self.find_nodespecs(nodeset):
            nodespec = NodeSpec(astnode, self.cur_line)
            nodespec.check_expression_usage()
            temp = self.nodespec_translation(nodespec)
            replacements.append((astnode, temp[0]))
            prologue += temp[1]
        return replacements, prologue

    def astnode_to_string(self, node):
        """
        Returns the portion of the current line which an ASTNode was parsed from.
        """
        return self.cur_line[node.start : node.end]

    def _with_replacements(self, text, offset, replacements):
        ret = ""
        start = 0
        for node, replacement in replacements:
            # print "Replacing", AST2XML(node)
            # print line
            # print rep
            ret += text[start:node.start - offset] + replacement
            start = max(0, node.end - offset)
        ret += text[start:]
        return ret

    def node_with_replacements(self, node, replacements):
        """
        Return the text for an ASTNode on the current line, with some descendant ASTNodes replaced with strings.
        """
        return self._with_replacements(self.cur_line[node.start : node.end], node.start, replacements)

    def cur_line_with_replacements(self, replacements):
        """
        Return a copy of the current line, with some ASTNodes replaced with strings.
        """
        return self._with_replacements(self.cur_line, 0, replacements)

    def output(self, text, prologue = ""):
        """
        Write something to the output (optionally with prologue with added indentation to match the current line)
        """
        if len(prologue):
            indentwith = whitespace.match(self.cur_line)
            prologue = indent(prologue, indentwith.group(0))
        self.outfile.write(prologue + text + "\n")

    def process_dim(self, node, indentwith = None):
        """
        Process a dimStatement
        """
        var_list = normalise_var_declarations(node)
        nodeptrs = self.find_nodeptr_vars(var_list)
        #print "Line", self.cur_filepos, "Found nodeptrs:", nodeptrs
        self.nodeptrs.extend(varname for (varname, initval) in nodeptrs)
        for varname, initval in nodeptrs:
            if initval:
                nodespec_node = initval.expression.get('nodeSpec')
                if nodespec_node:
                    self.set_derived_from(varname, get_ident(nodespec_node[0]))

        # Also check for nodespecs in initial values
        initvals = (initval for _, _, initval in var_list if initval)
        #print list(initvals)
        replacements, prologue = self.translate_nodespecs(initvals)

        if indentwith == None:
            indentwith = whitespace.match(self.cur_line).group(0)
            
        return indent(prologue, indentwith) + self.cur_line_with_replacements(replacements)

    def process_readnode_loadarray(self, node, nodespec, readnode, node_path):
        """
        Return the replacement for a LoadArray line inside a ReadNode, and add necessary
        array flushing to readnode.prologue.

        node is the loadArray ASTNode, nodespec is a preprocessed NodeSpec for the nodespec
        in the LoadArray's expression.
        """
        # At this point the child node name is stripped, so the index variable should be first
        assert nodespec.indices[0].name == 'identifier'
        del nodespec.indices[0]

        result = []
        _, replacement, prologue_ = self.simple_nodespec_translation(nodespec)
        if prologue_:
            result.append(prologue_)
        expression = self.node_with_replacements(node.expression, [(nodespec.node, replacement)])

        index_var = get_ident(node.identifier)
        arrayname = self.astnode_to_string(node.arrayName)

        # Prologue
        temp = FLUSH_ARRAY_TEMPLATE
        defaulted_expression = self.node_with_replacements(node.expression, [(nodespec.node, nodespec.get_default())])
        readnode.prologue += temp.format(tmpi = self.makename("_i"), array = arrayname, value = defaulted_expression)
        
        # Line translation
        result.append("'''" + self.cur_line.lstrip())
        temp = READNODE_LOADARRAY_TEMPLATE
        if nodespec.oob_error:
            args = {'warn_func' : self.error_func, 'error_extra' : " : " + self.exit}
        else:
            args = {'warn_func' : self.warn_func, 'error_extra' : ""}
        temp = temp.format(tmpi = index_var, node = nodespec.root_var, array = arrayname,
                           value = expression, codeloc = self.cur_filepos, nodepath = node_path, **args)
        result.append(temp)
        return result


    class ReadNode(object):
        def __init__(self, header, parent_nodeptr, context):
            """
            header is a readNode ASTNode.
            """
            self.ignoreall = "ignoreall" in header.what
            self.default = "default" in header.what

            self.prologue = ""
            # Names of child nodes seen so far
            self.children = set()
            # Children for which EITHER we need to do .warn or .required checks
            # OR we need to execute the line regardless of whether the node is present.
            # It is a list of (nodespec, self.cur_filepos, childname, always_run) tuples
            self.checks = []
            # Name of a bitarray
            self.check_array = context.makename("_seen")
            # The variable whose children we are iterating over
            self.parent_nodeptr = parent_nodeptr
            # The variable for iterating over the children
            self.child_nodeptr = context.makename("_ch")
            context.nodeptrs.append(self.child_nodeptr)
            context.set_derived_from(self.child_nodeptr, self.parent_nodeptr)


    def process_readnode_line(self, node, iterator, readnode, parent_node_path):
        """
        Process a readNode, withNode, or tokenList ASTNode (node) inside a readNode.
        """

        savescope = copy(self.users_temp_vars)

        if node.name == "readNode":
            if node[0].name == "identifier":
                raise LanguageError("Only the 'READNODE nodespec AS foo' form of ReadNode block can be nested inside a ReadNode", node)
            nodespec = NodeSpec(node.nodeSpec, self.cur_line)
            # Attribute checks are performed in process_readnode, shouldn't need to do them here
            #nodespec.check_header_usage("ReadNode header")
        elif node.name == "withNode":
            nodespec = NodeSpec(node.nodeSpec, self.cur_line)
            # Attribute checks are performed in process_withnode, shouldn't need to do them here
            #nodespec.check_header_usage("WithNode header")
        else:
            if node.name == "tokenList":
                index_vars = []
                nodespecs = self.find_nodespecs(node)
            elif node.name == "loadArray":
                index_vars = [get_ident(node.identifier)]
                nodespecs = self.find_nodespecs(node.expression)
            if len(nodespecs) != 1:
                raise LanguageError("Each line inside a ReadNode block should contain a single nodespec", node)
            nodespec = NodeSpec(nodespecs[0], self.cur_line, None, index_vars)
            if node.name == "tokenList":
                nodespec.check_readnode_expression_usage()
            elif node.name == "loadArray":
                nodespec.check_loadarray_usage()

        child = get_string(nodespec.indices[0])
        node_path = parent_node_path + "/" + child
        if nodespec.root_var.lower() != readnode.parent_nodeptr.lower():
            raise LanguageError("Each line inside this ReadNode block should have a nodespec rooted by the parent NodePtr, which is " + readnode.parent_nodeptr, nodespec.node[0])
        if child in readnode.children:
            note = ""
            if len(nodespec.indices) > 1:
                note = ". To refer to several descendants of a child, use a nested WithNode block for all of them"
            raise LanguageError("Child '" + child + "' appears more than once in this ReadNode block" + note, nodespec.indices[0])

        readnode.children.add(child)
        result = []
        case_comment = child #self.cur_line[nodespec.node.start:nodespec.node.end]

        always_run = (nodespec.type == "exists" or readnode.default or nodespec.default != None)
        # Whether to record this node's presence
        if node.name != "loadArray" and not nodespec.ignore and (always_run or nodespec.warn or nodespec.required):
            result.append("%s(%s) OR= 1 SHL %s\n" % (readnode.check_array, len(readnode.checks)/32, len(readnode.checks)%32))
            readnode.checks.append((nodespec, self.cur_filepos, child, always_run))

        if nodespec.ignore:
            if len(nodespec.indices) > 1:
                raise LanguageError("You can't ignore descendants, only children of " + readnode.parent_nodeptr + " inside this ReadNode block", nodespec.node)
            if readnode.ignoreall:
                print "%s: Warning: redundant .ignore inside an ignoreall ReadNode" % self.cur_filepos
            result.append("'ignore\n")
        else:
            # Note: invalidates nodespec._index_var_index
            del nodespec.indices[0]
            nodespec.root_var = readnode.child_nodeptr

            if node.name == "readNode":
                # We've already handled these
                nodespec = copy(nodespec)
                nodespec.required = False
                nodespec.warn = False
                # Reindent way too many times
                result.append(self.process_readnode(node, iterator, "", nodespec))
            elif node.name == "withNode":
                # We've already handled these
                nodespec = copy(nodespec)
                nodespec.required = False
                nodespec.warn = False
                result.append(self.process_withnode(node, iterator, "", nodespec))
            elif node.name == "tokenList":
                _, replacement, prologue_ = self.simple_nodespec_translation(nodespec)
                if prologue_:
                    result.append(prologue_)
                result.append(self.cur_line_with_replacements([(nodespec.node, replacement)]).lstrip() + "\n")
            elif node.name == "loadArray":
                # This also adds the array flushing to readnode.prologue
                result += self.process_readnode_loadarray(node, nodespec, readnode, node_path)

        #print self.cur_filepos, "restoring ", savescope, "(was", self.users_temp_vars, ")"
        self.users_temp_vars = savescope

        ret = "CASE %s: /'%s'/" % (self.intern_nodename(child), case_comment)
        #if len(result) == 1:
        #    return ret + "  " + result[0]
        return ret + "\n" + indent(result, "      ")

    def process_readnode(self, header, iterator, indentwith = None, override_nodespec = None):
        """
        Process a whole readnode block; header is a readNode ASTNode.
        override_nodespec may be passed to provided a modified nodespec
        """

        output = ["'''" + self.cur_line.lstrip() + "\n"]
        prologue_ = ""

        if header[0].name == "nodeSpec":
            # READNODE nodespec AS identifier [, ignoreall] [, default]

            if override_nodespec:
                nodespec = override_nodespec
            else:
                nodespec = NodeSpec(header[0], self.cur_line, "ptr")
            nodespec.check_header_usage("ReadNode header")

            # block_nodespec_translation handles .warn and .required attributes. We
            # don't skip over the WHILE block, <s>but that's OK because FirstChild
            # returns NULL for a NULL NodePtr</s>.
            #translation, prologue = self.nodespec_translation(nodespec, True)

            parent_nodeptr = get_ident(header[1])
            prologue_ = self.block_nodespec_translation(nodespec, parent_nodeptr)
            node_path = nodespec.path_string()

        else:
            # READNODE identifier [, ignoreall] [, default]
            parent_nodeptr = get_ident(header[0])
            if parent_nodeptr.lower() not in self.nodeptrs:
                raise LanguageError("ReadNode block parent node not recognised as a NodePtr variable", header[0])
            node_path = parent_nodeptr + ":"

        readnode = ReloadBasicFunction.ReadNode(header, parent_nodeptr, self)
        readnode.prologue = prologue_

        nametable, declare_table, build_table = self.nameindex_table(readnode.parent_nodeptr)
        build_table = indent(build_table, "  ")
        output.append(declare_table)

        select_cases = []

        for lineno, line, node in iterator:
            if iterator.line_is_blank():
                select_cases.append(line.lstrip() + "\n")
                continue

            nodetype = node.name
            if nodetype == "readNodeEnd":
                break
            elif nodetype in ("tokenList", "readNode", "withNode", "loadArray"):
                case = self.process_readnode_line(node, iterator, readnode, node_path)
                select_cases.extend(line + "\n" for line in case.split("\n"))
            else:
                raise ParseError("Unexpected inside a ReadNode block:\n" + line)

        if len(readnode.checks):
            readnode.prologue += "DIM %s(%s) as uinteger\n" % (readnode.check_array, len(readnode.checks) / 32)

        output.append(readnode.prologue)

        nameindex_var = self.makename("_nameidx")

        need_loopback = False

        checks_text = []
        for i, (nodespec, filepos, child, always_run) in enumerate(readnode.checks):
            msg = 'IF (%s(%s) AND (1 SHL %s)) = 0 THEN' % (readnode.check_array, i/32, i%32)
            msg2 = ' {func} "%s:{what} Did not see expected node %s/%s"' % (filepos, node_path, child)

            if nodespec.required:
                msg += msg2.format(func = self.error_func, what = " Error:") + " : " + self.exit
            else:
                if nodespec.warn:
                    msg += msg2.format(func = self.warn_func, what = "")
                if always_run:
                    if not msg.endswith("THEN"):
                        msg += " :"
                    msg += " %s = %s : CONTINUE DO"  % (nameindex_var, self.global_scope.nameindex(child))
                    need_loopback = True
            checks_text.append(msg +  "\n")

        if not readnode.ignoreall:
            if need_loopback:
                # Don't need to include if there's no CASE ELSE
                # I would use CASE -1, but FB forbids it... crazy
                select_cases.append("CASE INVALID_INDEX:  'Only if %s has no children\n" % readnode.parent_nodeptr)
            select_cases.append('CASE ELSE:  %s "%s: unexpected node %s/" & *%s->name' % (self.warn_func, self.cur_filepos, node_path, readnode.child_nodeptr))
        select_cases = "".join("    " + c for c in select_cases)

        checks_text = "".join(checks_text)
        if need_loopback:
            checks_text = indent(checks_text, "  ")
            out = READNODE_DEFAULTS_TEMPLATE
        else:
            out = READNODE_TEMPLATE
        out = out.format(it = readnode.child_nodeptr, node = readnode.parent_nodeptr, nameindex = nameindex_var,
                         buildtable = build_table, nametbl = nametable, cases = select_cases, checks = checks_text)
            
        output.append(out)
        output.append("'''" + self.cur_line.lstrip() + "\n")

        if indentwith == None:
            indentwith = whitespace.match(self.cur_line).group(0)
        return indent("".join(output), indentwith)

    def process_withnode(self, header, iterator, indentwith = None, override_nodespec = None):
        """
        Process a whole withnode block; header is a withNode ASTNode.
        override_nodespec may be passed to provided a modified NodeSpec
        """
        if override_nodespec:
            nodespec = override_nodespec
        else:
            nodespec = NodeSpec(header[0], self.cur_line, "ptr")
        nodespec.check_header_usage("WithNode header")

        # block_nodespec_translation handles .warn and .required attributes. We
        # don't skip over the WHILE block, <s>but that's OK because FirstChild
        # returns NULL for a NULL NodePtr</s>.
        #translation, prologue = self.nodespec_translation(nodespec, True)

        lines = ["'''" + self.cur_line.lstrip()]

        parent_nodeptr = get_ident(header[1])
        lines.append(self.block_nodespec_translation(nodespec, parent_nodeptr))

        savescope = copy(self.users_temp_vars)

        for lineno, line, node in iterator:
            if iterator.line_is_blank():
                lines.append(line)
                continue

            nodetype = node.name
            #print nodetype
            if nodetype == "dimStatement":
                lines.append(self.process_dim(node, ""))
            elif nodetype == "tokenList":
                replacements, prologue = self.translate_nodespecs(node)
                #print "tokens", replacements, repr(prologue)
                lines.append(prologue + self.cur_line_with_replacements(replacements))
            elif nodetype == "directive":
                # warn_func or error_func
                setattr(self, node[0].lower(), get_ident(node[1]))
            elif nodetype == "readNode":
                lines.append(self.process_readnode(node, iterator, ""))
            elif nodetype == "withNode":
                lines.append(self.process_withnode(node, iterator, ""))
            elif nodetype == "withNodeEnd":
                lines.append("'''" + line.lstrip())
                break
            else:
                raise ParseError("Unexpected inside a WithNode block:\n" + line)

        self.users_temp_vars = savescope

        if indentwith == None:
            indentwith = whitespace.match(self.cur_line).group(0)
        return "".join(indentwith + l + "\n" for l in lines)
    
    def process_function(self, header, iterator):
        """
        header is a subStart or functionStart ASTNode.
        """
        self.name = get_ident_with_case(header.identifier)

        args = header.get("argList")
        if args:
            nodeptr_args = self.find_nodeptr_vars(normalise_typed_var_list(args))
            self.nodeptrs.extend(varname for (varname, initval) in nodeptr_args)
        #print "Found nodeptrs in args:", local_nodeptrs

        if header.name == "subStart":
            self.exit = "EXIT SUB"
        else:
            self.exit = "RETURN 0"

        self.start_mark = self.outfile.get_mark()

        iterator.hook = self
        for lineno, line, node in iterator:
            if iterator.line_is_blank():
                self.output(line)
                continue

            #self.used_temp_vars = 0

            nodetype = node.name
            #print nodetype
            if nodetype == "dimStatement":
                self.output(self.process_dim(node))
            elif nodetype == "tokenList":
                replacements, prologue = self.translate_nodespecs(node)
                #print "tokens", replacements, repr(prologue)
                self.output(self.cur_line_with_replacements(replacements), prologue)
            elif nodetype == "directive":
                # warn_func or error_func
                setattr(self, node[0].lower(), get_ident(node[1]))
            elif nodetype == "readNode":
                self.output(self.process_readnode(node, iterator))
            elif nodetype == "withNode":
                self.output(self.process_withnode(node, iterator))
            #elif nodetype == "loadArray":
            #    self.output(self.process_loadarray(node, iterator))
            elif nodetype in ("subEnd", "functionEnd"):
                self.output(line)
                iterator.hook = None
                break
            else:
                raise ParseError("Unexpected inside a function/sub:\n" + line)

        if len(self.nodenames):
            out = "STATIC _nodenames(...) as RBNodeName => {%s}\n"
            nodenames = ((self.global_scope.nameindex(name), reload_HashZString(name), name) for name in self.nodenames)
            out = out % ", ".join('(%s, %s, @"%s")' % n for n in nodenames)
            self.start_mark.write(out)


class ReloadBasicTranslator(object):
    def __init__(self, xml_dump = False, be_careful = False):
        self.nodenames = {}
        self.xml_dump = xml_dump
        self.be_careful = be_careful

        self.warn_func = "debug"
        self.error_func = "debug"

    def nameindex(self, nodename):
        """
        Assign each node name an increasing positive integer which is unique in this source file.
        """
        return self.nodenames.setdefault(nodename, len(self.nodenames) + 1)

    def process_file(self, filename, outfilename = ""):
        if outfilename == "":
            outfilename = os.path.splitext(filename)[0] + ".bas"
        if outfilename == filename:
            sys.exit("Refusing to overwrite input file with output")
        outfile = DelayedFileWriter(open(outfilename, 'w'))

        self.magic_number = randint(1, 2000000000)
        outfile.write('#define RELOADINTERNAL\n')
        outfile.write('#include "reload.bi"\n')
        outfile.write('#include "reloadext.bi"\n')
        outfile.write('#include "util.bi"\n')  # for iif_string
        outfile.write('USING Reload\n')
        outfile.write('USING Reload.Ext\n')
        outfile.write('\n')
        outfile.write("#define RB_SIGNATURE %s  'hopefully unique to this file\n" % self.magic_number)
        header_mark = outfile.get_mark()
        self.num_functions = 0

        iterator = TranslationIteratorWrapper(filename, self.xml_dump)
        for lineno, line, node in iterator:
            try:
                if iterator.line_is_blank():
                    outfile.write(line + "\n")
                    continue

                nodetype = node.name
                #print nodetype
                if nodetype in ("subStart", "functionStart"):
                    outfile.write(line + "\n")
                    translator = ReloadBasicFunction(outfile, filename, self.num_functions, self, self.be_careful, self.warn_func, self.error_func)
                    translator.process_function(node, iterator)
                    self.num_functions += 1
                elif nodetype == "dimStatement":
                    outfile.write(line + "\n")
                elif nodetype == "directive":
                    # warn_func or error_func
                    setattr(self, node[0].lower(), get_ident(node[1]))
                elif nodetype == "tokenList":
                    outfile.write(line + "\n")
                else:
                    raise ParseError("Found unexpected " + nodetype + " outside any function")

            except ParseError, e:
                if hasattr(e, "node"):
                    e.message += "\n" + pyPEG.pointToError(iterator.line, e.node.start, e.node.end)
                print "On line %d of %s:\n%s" % (iterator.lineno, filename, str(e))
                sys.exit(1)

        #header_mark.write("#define NUM_RB_FUNCS %s\n" % self.num_functions)
        header_mark.write("#define RB_FUNC_BITS_ARRAY_SZ %s\n" % ((self.num_functions / 32 + 1) * 4))
        header_mark.write("#define RB_NUM_NAMES %s\n" % (len(self.nodenames)))
        header_mark.write("#define INVALID_INDEX %s\n" % (len(self.nodenames) + 1))
        outfile.flush()


################################################################################


if __name__ == "__main__":
    parser = optparse.OptionParser(usage="%prog [options] [-o outfile.bas] infile.rbas", description="Translate a " + reloadbasic + " source file to a FreeBASIC source file.")
    parser.add_option("-o", dest="outfile", default="",
                      help="the name of the output file, defaults to <infile>.bas")
    parser.add_option("-t", "--trace", action="store_true", dest="trace", default=False,
                      help="output a parser debugging trace to stderr")
    parser.add_option("-x", "--xml", action="store_true", dest="xml", default=False,
                      help="dump AST tree of each source line to stderr as XML")
    parser.add_option("-c", "--careful", action="store_true", dest="careful", default=False,
                      help="generate cautious (and slightly slower) code which makes fewer assumptions about which documents NodePtr variables belong to")

    (options, args) = parser.parse_args()
    if len(args) != 1:
        parser.print_help()
        print
        sys.exit("Error: Expected exactly one input file.")
    pyPEG.print_trace = options.trace

    translator = ReloadBasicTranslator(options.xml, options.careful)
    translator.process_file(args[0], options.outfile)
