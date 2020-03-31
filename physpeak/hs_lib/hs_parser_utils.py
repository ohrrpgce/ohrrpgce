"""
Functions for inspecting a ply.yacc.LRParser's internal state, and to report
syntax errors.
"""

import logging
from hs_ast import AST_state
import hs_parse

def expected_terms_and_nonterms(parser):
    """Return a pair, the sets of terminals and nonterminals that are valid here.
    One or both of them might be empty if they couldn't be determined."""

    state = parser.state  # state ID

    logging.debug("p_error: state %d actions %s", state, parser.action[state])
    if state in parser.goto:
        logging.debug("p_error: state %d goto %s", state, parser.goto[state])
    else:
        logging.debug("p_error: state %d no goto", state)
    logging.debug("p_error: stack %s", parser.symstack)

    if state in parser.defaulted_states:
        # I think this should never happen when called from p_error
        print("Internal error: expected_terms_and_nonterms: state %d defaults to action %d"
              % (state, parser.defaulted_states[state]))
        return set(), set()

    # action maps token type (str) to:
    #  0: stop
    # >0: new state ID (shift symbol onto symstack)
    # <0: negated production ID (reduce and lookup new state in parser.goto[initial_prod_state])
    actions = parser.action[state]
    terms = set(actions.keys())

    terms.discard("error")

    nonterms = set()
    if state in parser.goto:
        # The parser doesn't actually read goto[state] at this point, but will
        # do so in future once it's shifted all the symbols needed to reduce a
        # production that begins here. (goto[] maps from the name of the
        # completed production to a new state.)
        all_nonterms = parser.goto[state].keys()
        logging.info("[[]] All nonterms: %s" % list(all_nonterms))

        # The list of nonterms contains a lot of redundancy because if nonterm A in the list
        # can begin with B then B appears too. Remove these non-root nonterms.
        nonterms = set(all_nonterms)
        for prod in parser.productions:
            if prod.name in all_nonterms:
                # If the parser tables are loaded from a cache then the only way
                # get the RHS of the production is from the description string
                rhs_syms = prod.str.split('-> ')[1].split(' ')
                if len(rhs_syms):
                    first_sym = rhs_syms[0]
                    if first_sym != prod.name:
                        #if first_sym in nonterms: print(" nonprim:", prod.str)
                        nonterms.discard(first_sym)

    return terms, nonterms


def describe_parser_expectation(parser):
    "Returns a string describing what the parser was expecting to see at this point"

    terms, raw_nonterms = expected_terms_and_nonterms(parser)

    # Rename nonterminal symbols to user-friendly names
    nonterms = set()
    for nonterm in raw_nonterms:
        if nonterm in hs_parse.symdesc:
            nonterms.add(hs_parse.symdesc[nonterm])
        else:
            nonterms.add(nonterm.replace('_', ' '))

    if '$end' in terms:
        terms.discard('$end')
        terms.add('end-of-input')

    # Special case for expressions
    if terms.issuperset(hs_parse.operator_list):
        terms.difference_update(hs_parse.operator_list)
        terms = list(terms) + [" an operator like +"]  # Make sure it's listed last

    # Only show terms if there aren't too many, which happens if an arbitrary statement can go here
    if len(terms) > 5:
        terms = []

    if len(nonterms) == 0 and len(terms) == 0:
        return ""

    def if_multiple(items, text):
        if len(items) > 1:
            return text
        return ""

    msg = "Expected to see"
    if len(nonterms):
        msg += if_multiple(nonterms, " one of") + " " + ", ".join(nonterms)
        if len(terms):
            msg += " beginning with"
    if len(terms):
        msg += if_multiple(terms, " one of") + ": " + " ".join(terms)
    return msg


def tell_error(p, erridx, msg = "", tell_token = False):
    """Log an error and show the line where it occurred.

    erridx should generally be the bad token index in p for rules that throw
    SyntaxError, and should be 0 for error-recovery rules containing 'error',
    to indicate the whole production, because p_error() would already have
    indicated the bad token.

    tell_token: tell expection, possibly useful if throwing a SyntaxError, in
    which case p_error doesn't run to do it automatically.
    """
    if tell_token:
        try:
            obj = p[erridx].value
        except:
            obj = p[erridx]
        if msg:
            msg = ": " + msg
        syntax_msg = "Syntax error at '%s'" % (obj,)
        extra_msg = describe_parser_expectation(parser)
        if extra_msg:
            syntax_msg = ": " + extra_msg
        AST_state.add_error(p.lexspan(erridx), p.lineno(erridx), syntax_msg)
        erridx = 0

    caret = '^'
    if erridx == 0:
        caret = '~' 

    span = p.lexspan(erridx)
    # if span[1] > span[0]:
    #     # The end point of the span will be where the bad token starts
    #     span = span[0], span[1] - 1
    AST_state.add_error(span, p.lineno(erridx), msg, caret)
