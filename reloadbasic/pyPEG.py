# coding=utf-8
# A fork of pyPEG by Volker Birk, licensed under the GNU GPL v2
# 
# Changelog:
#            1.4:   Initial version from http://fdik.org/pyPEG
# 2011-11-15 1.4.1: * Added tracking of start & end of text matching a Symbol
#                   * Fixed "except: pass"s which broke memorization and more
#                   * Added checkpoints along with a proper error reporting system;
#                     throws a detailed ParseError instead of SyntaxError.
#                   * Added forceKeywords option
#                   (Ralph Versteegen)
# 2011-11-17 1.4.2  * Breaking changes in AST structure: ASTNode replaces Symbol, Name
#                   * LineParser class replaces parseLine function
#                   * Added caseInsensitive option
#                   (Ralph Versteegen)
# 2020-02-28 1.4.3: Support both Python 2 and 3. Py2 Unicode support untested
#                   (Daniel Würl, Ralph Versteegen)
# 2022-05-22 1.4.4: Python 3 fixes

import re
import sys

if sys.version_info.major == 2:
    StringTypes = (str, unicode)
else:
    StringTypes = (str,)

word_regex = re.compile(r"\w+")
whole_word_regex = re.compile(r"\w+$")
rest_regex = re.compile(r".*")

class keyword(str): pass
class code(str): pass
class ignore(object):
    def __init__(self, display, regex_text, flags = 0):
        """display is what is used for description in error messages.
        regex_text is compiled to the actual regex"""
        self.regex = re.compile(regex_text, flags)
        self.regex_text = regex_text
        self.display = display

class _and(object):
    def __init__(self, something):
        self.obj = something

class _not(_and): pass

class ASTNode(object):
    def __init__(self, name, what):
        self.name = name
        self.what = what
    def __getitem__(self, key):
        return self.what[key]
    def get(self, key, default = None):
        for node in self.what:
            if isinstance(node, ASTNode) and node.name == key:
                return node
        return default
    def __getattr__(self, key):
        for node in self.what:
            if isinstance(node, ASTNode) and node.name == key:
                return node
        raise KeyError()
    def __iter__(self):
        return iter(self.what)
    def __len__(self):
        return len(self.what)
    def __call__(self):
        return self.what
    def __eq__(self, rhs):
        return isinstance(rhs, ASTNode) and self.name == rhs.name and self.what == rhs.what
    def __str__(self):
        return 'ASTNode(' + repr(self.name) + ', ' + repr(self.what) + ')'
    def __repr__(self):
        return str(self)


class ParseError(Exception):
    def __init__(self, message):
        self.message = message
    def __str__(self):
        return self.message

class FatalParseError(ParseError):
    "Non-backtrackable parsing failure"
    def __init__(self, message = "", offset = 0, expected = None):
        self.message = message
        self.offset = offset
        if expected:
            self.message += self.describePattern(expected)

    def describePattern(self, expected):
        while type(expected) == tuple:
            n = 0
            while type(expected[n]) == int:
                if expected[n] in (0, -1):
                    n += 2
                else:
                    n += 1
            expected = expected[0]
        if isinstance(expected, StringTypes):  # includes keywords
            return "'" + expected + "'"
        elif isinstance(expected, list):
            return "one of: " + ", ".join(self.describePattern(elem) for elem in expected)
        elif type(expected) == type(word_regex):
            return "<Regex>"
        elif type(expected) == ignore:
            return expected.display
        elif callable(expected):
            return str(expected.__name__)

class ParseFailure(ParseError):
    "Failure to match a pattern"
    def __init__(self, offset = 0):
        self.offset = offset

print_trace = False

def skip(skipper, text, skipWS, skipComments):
    if skipWS:
        t = text.lstrip()
    else:
        t = text
    if skipComments:
        try:
            while True:
                skipper.last_comment, t = skipper.parseLine(t, skipComments, [], skipWS, None)
                if skipWS:
                    t = t.lstrip()
        except ParseFailure:
            pass
    return t

class parser(object):
    def __init__(self, another = False, p = False, forceKeywords = False, caseInsensitive = False):
        self.restlen = -1 
        if not(another):
            self.skipper = parser(True, p)
            self.skipper.packrat = p
        else:
            self.skipper = self
        self.lines = None
        self.textlen = 0
        self.memory = {}
        self.packrat = p
        self.patternCache = {}
        self.keywordCache = {}
        self.forceKeywords = forceKeywords
        self.last_comment = None
        self.caseInsensitive = caseInsensitive

    def transformPattern(self, pattern):
        """
        If needed, convert all strings within this pattern to keyword instances (if they look like keywords),
        and/or make things case insensitive.
        """
        if isinstance(pattern, StringTypes):
            # This cache is not to speed up transformPattern (the result is cached anyway),
            # instead it's used so that identical patterns are transformed to the same pattern,
            # improving memoization
            if pattern in self.keywordCache:
                return self.keywordCache[pattern]
            makekeyword = isinstance(pattern, keyword)  # Because keyword subclasses unicode
            if self.forceKeywords:
                makekeyword = makekeyword or whole_word_regex.match(pattern)
            if self.caseInsensitive:
                if makekeyword:
                    ret = ignore("'%s'" % pattern, re.escape(pattern) + "(?!\w)", re.I)
                else:
                    ret = ignore("'%s'" % pattern, re.escape(pattern), re.I)
            elif makekeyword:
                ret = keyword(pattern)
            else:
                ret = pattern
            self.keywordCache[pattern] = ret
            return ret
        elif isinstance(pattern, ignore) and self.caseInsensitive:
            if pattern.regex_text in self.keywordCache:
                return self.keywordCache[pattern.regex_text]
            pattern.regex = re.compile(pattern.regex_text, re.I)
            self.keywordCache[pattern.regex_text] = pattern
            return pattern
        elif hasattr(pattern, '__iter__'):
            return type(pattern)(self.transformPattern(elem) for elem in pattern)
        else:
            return pattern

    # parseLine():
    #   textline:       text to parse
    #   pattern:        pyPEG language description
    #   resultSoFar:    parsing result so far (default: blank list [])
    #   skipWS:         Flag if whitespace should be skipped (default: True)
    #   skipComments:   Python functions returning pyPEG for matching comments
    #   offset:         The nominal offset of the beginning of textline (normally 0)
    #   rulename:       The Name of the rule containing the current subpattern
    #   
    #   returns:        pyAST, textrest
    #
    #   raises:         ParseFailure(offset) if textline is detected not being in language
    #                   described by pattern
    #
    #                   FatalParseError(reason, offset, expected) as above, but backtracking is prevented
    #
    #                   SyntaxError(reason) if pattern is an illegal language description

    def parseLine(self, textline, pattern, resultSoFar = [], skipWS = True, skipComments = None, offset = 0, rulename = ""):
        name = None
        _textline = textline
        _pattern = pattern

        def R(result, text):
            if __debug__:
                if print_trace:
                    if hasattr(_pattern, '__name__'):
                        if _pattern.__name__ != "comment":
                            sys.stderr.write("match: " + _pattern.__name__ + "\n")

            if self.restlen == -1:
                self.restlen = len(text)
            else:
                self.restlen = min(self.restlen, len(text))
            res = resultSoFar
            if name:
                if result:
                    node = ASTNode(name, result)
                else:
                    node = ASTNode(name, [])
                node.start = offset
                node.end = offset + text_start_len - len(text)
                #node.lineno = self.lineNo()
                res.append(node)
            elif result:
                if type(result) is type([]):
                    res.extend(result)
                else:
                    res.extend([result])
            if self.packrat:
                self.memory[(len(_textline), id(_pattern))] = (res, text)
            return res, text

        def syntaxError():
            if self.packrat:
                self.memory[(len(_textline), id(_pattern))] = False
            raise ParseFailure(offset + text_start_len - len(text))

        if self.packrat:
            try:
                result = self.memory[(len(textline), id(pattern))]
                if result:
                    return result
                else:
                    raise ParseFailure(offset)
            except KeyError:
                pass

            # Assuming self.skipper has identical packrat setting
            try:
                text = self.skipper.memory[len(textline)]
            except KeyError:
                text = skip(self.skipper, textline, skipWS, skipComments)
                self.skipper.memory[len(textline)] = text
        else:
            text = skip(self.skipper, textline, skipWS, skipComments)
        text_start_len = len(text)
        offset += len(textline) - text_start_len

        if callable(pattern):
            if __debug__:
                if print_trace:
                    if hasattr(_pattern, '__name__'):
                        if pattern.__name__ != "comment":
                            sys.stderr.write("testing with " + pattern.__name__ + ": " + textline[:40] + "\n")

            if pattern.__name__[0] != "_":
                name = pattern.__name__
                rulename = name

            try:
                pattern = self.patternCache[_pattern]
            except KeyError:
                pattern = pattern()
                if self.forceKeywords or self.caseInsensitive:
                    pattern = self.transformPattern(pattern)
                if callable(pattern):
                    pattern = (pattern,)
                self.patternCache[_pattern] = pattern


        pattern_type = type(pattern)

        if pattern_type in StringTypes:
            if text.startswith(pattern):
                text = text[len(pattern):]
                return R(None, text)
            else:
                syntaxError()

        elif pattern_type is keyword:
            m = word_regex.match(text)
            if m and m.group() == pattern:
                text = text[len(pattern):]
                return R(None, text)
            syntaxError()

        elif pattern_type is _not:
            try:
                r, t = self.parseLine(text, pattern.obj, [], skipWS, skipComments, offset, rulename)
            except ParseFailure:
                return resultSoFar, textline
            syntaxError()

        elif pattern_type is _and:
            r, t = self.parseLine(text, pattern.obj, [], skipWS, skipComments, offset, rulename)
            return resultSoFar, textline

        elif pattern_type is type(word_regex) or pattern_type is ignore:
            if pattern_type is ignore:
                pattern = pattern.regex
            m = pattern.match(text)
            if m:
                text = text[m.end():]
                if pattern_type is ignore:
                    return R(None, text)
                else:
                    return R([m.group()], text)
            else:
                syntaxError()

        elif pattern_type is tuple:
            result = []
            n = 1
            checkpointed = False
            newOffset = offset
            for p in pattern:
                if type(p) is type(0):
                    if p>-3:
                        n = p
                    elif p==-3:
                        checkpointed = True
                        # This only throws out memoized results we might use again if we're inside a _not or _and
                        #self.memory = {}
                    else:
                        raise SyntaxError("unrecognised integer in grammar: " + str(p))
                else:
                    if n>0:
                        try:
                            for i in range(n):
                                result, newText = self.parseLine(text, p, result, skipWS, skipComments, newOffset, rulename)
                                newOffset += len(text) - len(newText)
                                text = newText
                        except ParseFailure as e:
                            if checkpointed:
                                raise FatalParseError("while parsing " + rulename + ", expected ", e.offset, expected = p)
                            raise
                    elif n==0:
                        if text == "":
                            pass
                        else:
                            try:
                                result, newText = self.parseLine(text, p, result, skipWS, skipComments, newOffset, rulename)
                                newOffset += len(text) - len(newText)
                                text = newText
                            except ParseFailure:
                                pass
                    elif n>=-2:
                        found = False
                        while True:
                            try:
                                result, newText = self.parseLine(text, p, result, skipWS, skipComments, newOffset, rulename)
                                newOffset += len(text) - len(newText)
                                text, found = newText, True
                            except ParseFailure:
                                break
                        if n == -2 and not(found):
                            if checkpointed:
                                raise FatalParseError("while parsing " + rulename + ", expected ", newOffset, expected = p)
                            syntaxError()
                    n = 1
            return R(result, text)

        elif pattern_type is list:
            result = []
            found = False
            for p in pattern:
                try:
                    result, text = self.parseLine(text, p, result, skipWS, skipComments, offset, rulename)
                    found = True
                except ParseFailure:
                    pass
                if found:
                    break
            if found:
                return R(result, text)
            else:
                syntaxError()

        else:
            raise SyntaxError("illegal type in grammar: " + str(pattern_type))

    def lineNo(self):
        if not(self.lines): return ""
        if self.restlen == -1: return ""
        parsed = self.textlen - self.restlen

        left, right = 0, len(self.lines)

        while True:
            mid = (right + left) // 2
            if self.lines[mid][0] <= parsed:
                try:
                    if self.lines[mid + 1][0] >= parsed:
                        try:
                            return "%s:%d" % (self.lines[mid + 1][1], self.lines[mid + 1][2])
                        except:
                            return ""
                    else:
                        left = mid + 1
                except:
                    try:
                        return "%s:%d" % (self.lines[mid + 1][1], self.lines[mid + 1][2])
                    except:
                        return ""
            else:
                right = mid - 1
            if left > right:
                return ""

def visualColumn(text, offset):
    """
    Assuming that tabs are 8 spaces, returns the column that a certain character of a string is displayed at
    """
    ret = 0
    for c in text[:offset]:
        if c == "\t":
            ret = (ret // 8 + 1) * 8
        else:
            ret += 1
    return ret

def pointToError(text, offset1, offset2 = None):
    message = text
    if not message.endswith("\n"):
        message += "\n"
    col1 = visualColumn(text, offset1)
    col2 = col1 + 1
    if offset2 != None:
        col2 = visualColumn(text, offset2)
    return message + " " * col1 + "^" * max(1, col2 - col1)


# plain module API

class LineParser(object):
    def __init__(self, skipWS = True, skipComments = None, packrat = False, forceKeywords = False, caseInsensitive = False):
        self.p = parser(p = packrat, forceKeywords = forceKeywords, caseInsensitive = caseInsensitive)
        self.skipWS = skipWS
        self.skipComments = skipComments

    def parse_line(self, textline, pattern, matchAll = False, lineinfo = None, offset = 0):
        if lineinfo:
            self.p.lines = lineinfo
        self.p.memory = {}
        self.p.skipper.memory = {}
        self.p.skipper.last_comment = [None]
        # Preserve other caches
        try:
            ast, text = self.p.parseLine(textline, pattern, [], self.skipWS, self.skipComments, offset)
            text = skip(self.p.skipper, text, self.skipWS, self.skipComments)
            if matchAll and len(text) > 0:
                raise FatalParseError("garbage at end of line", len(textline) - len(text))
        except ParseError as e:
            e.message = "Syntax error: " + e.message + "\n" + pointToError(textline, e.offset)
            raise e
        return ast, text

    def last_comment(self):
        return self.p.skipper.last_comment[0]

# parse():
#   language:       pyPEG language description
#   lineSource:     a fileinput.FileInput object
#   skipWS:         Flag if whitespace should be skipped (default: True)
#   skipComments:   Python function which returns pyPEG for matching comments
#   packrat:        use memoization
#   lineCount:      add line number information to AST
#   forceKeywords:  all strings composed of alphanumeric characters are automatically treated as keywords
#   
#   returns:        pyAST
#
#   raises:         ParseError(reason), if a parsed line is not in language
#                   SyntaxError(reason), if the language description is illegal

def parse(language, lineSource, skipWS = True, skipComments = None, packrat = False, lineCount = True, forceKeywords = False, caseInsensitive = False):
    lines, lineNo = [], 0

    while callable(language):
        language = language()

    is_py2 = (sys.version_info.major == 2)
    orig, ld = "", 0
    for line in lineSource:
        if lineSource.isfirstline():
            ld = 1
        else:
            ld += 1
        lines.append((len(orig), lineSource.filename(), lineSource.lineno() - 1))
        if is_py2:
            line = unicode(line)
        orig += line

    textlen = len(orig)

    try:
        p = parser(p = packrat, forceKeywords = forceKeywords, caseInsensitive = caseInsensitive)
        p.textlen = len(orig)
        if lineCount:
            p.lines = lines
        else:
            p.line = None
        result, text = p.parseLine(orig, language, [], skipWS, skipComments)
        text = skip(p.skipper, text, skipWS, skipComments)
        if text:
            raise FatalParseError("garbage at end of line", len(orig) - len(text))

    except ParseError as e:
        parsed = textlen - p.restlen
        textlen = 0
        nn, lineNo, file = 0, 0, ""
        for n, ld, l in lines:
            if n >= parsed:
                break
            else:
                lineNo = l
                nn += 1
                file = ld

        lineNo += 1
        nn -= 1
        lineCont = orig.splitlines()[nn]
        column = e.offset - lines[nn][0]

        e.message = "Syntax error at %s:%s:%s: %s\n%s" % (
            file, lineNo, column, e.message, pointToError(lineCont, column))
        raise e

    return result
