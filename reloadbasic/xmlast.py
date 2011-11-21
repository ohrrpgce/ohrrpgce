from xml.sax.saxutils import escape
from pyPEG import ASTNode

def _pyAST2XML(pyAST, indent = 0, forcenl = False, realxml = False):
    space = u"  " * indent
    if isinstance(pyAST, unicode) or isinstance(pyAST, str):
        result = space + escape(pyAST)
        if forcenl:
            result += "\n"
        return result
    if type(pyAST) is ASTNode:
        result = space + u"<" + pyAST.name.replace("_", "-")
        if realxml:
            result += u' start="%s" end="%s">' % (pyAST.start, pyAST.end)
        else:
            result += u" start=%s end=%s>" % (pyAST.start, pyAST.end)
        if len(pyAST.what) == 1 and type(pyAST.what[0]) in (unicode, str):
            result += escape(pyAST.what[0])
        else:
            result += "\n"
            for e in pyAST:
                result += _pyAST2XML(e, indent + 1, True, realxml)
            result += space
        result += u"</" + pyAST.name.replace("_", "-") + u">\n"
    else:
        result = u""
        for e in pyAST:
            result += _pyAST2XML(e, indent + 1, False, realxml)
        result += "\n"
    return result

def AST2XML(pyAST, realxml = False):
    "Return XML representation of an ASTNode. Outputs valid XML only if realxml is passed True."
    return _pyAST2XML(pyAST, 0, False, realxml)
