from xml.sax.saxutils import escape
from pyPEG import Symbol

def _pyAST2XML(pyAST, indent = 0, forcenl = False):
    space = u"  " * indent
    if isinstance(pyAST, unicode) or isinstance(pyAST, str):
        result = space + escape(pyAST)
        if forcenl:
            result += "\n"
        return result
    if type(pyAST) is Symbol:
        result = space + u"<" + pyAST[0].replace("_", "-") + u">"
        if isinstance(pyAST[1], unicode) or isinstance(pyAST[1], str):
            result += escape(pyAST[1])
        else:
            result += "\n"
            for e in pyAST[1]:
                result += _pyAST2XML(e, indent + 1, True)
            result += space
        result += u"</" + pyAST[0].replace("_", "-") + u">\n"
    else:
        result = u""
        for e in pyAST:
            result += _pyAST2XML(e, indent + 1, False)
        result += "\n"
    return result

def pyAST2XML(pyAST):
    return _pyAST2XML(pyAST)
