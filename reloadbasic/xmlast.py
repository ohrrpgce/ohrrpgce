import sys
from xml.sax.saxutils import escape
from pyPEG import ASTNode

if sys.version_info.major == 2:
    StringTypes = (str, unicode)
else:
    StringTypes = (str,)

def _pyAST2XML(pyAST, indent = 0, forcenl = False, realxml = False):
    space = "  " * indent
    if isinstance(pyAST, StringTypes):
        result = space + escape(pyAST)
        if forcenl:
            result += "\n"
        return result
    if type(pyAST) is ASTNode:
        result = space + "<" + pyAST.name.replace("_", "-")
        if realxml:
            result += ' start="%s" end="%s">' % (pyAST.start, pyAST.end)
        else:
            result += " start=%s end=%s>" % (pyAST.start, pyAST.end)
        if len(pyAST.what) == 1 and isinstance(pyAST.what[0], StringTypes):
            result += escape(pyAST.what[0])
        else:
            result += "\n"
            for e in pyAST:
                result += _pyAST2XML(e, indent + 1, True, realxml)
            result += space
        result += "</" + pyAST.name.replace("_", "-") + ">\n"
    else:
        result = ""
        for e in pyAST:
            result += _pyAST2XML(e, indent + 1, False, realxml)
        result += "\n"
    return result

def AST2XML(pyAST, realxml = False):
    "Return XML representation of an ASTNode. Outputs valid XML only if realxml is passed True."
    return _pyAST2XML(pyAST, 0, False, realxml)
