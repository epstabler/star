""" file: pptree.py  stabler@ucla.edu
    pretty print a tree given in list format
"""
def pptree(n, t):
    """ pretty print t indented n spaces. E.g.
          pptree(0, ['TP', ['DP', ['John']], ['VP', ['V',['praises']], ['DP', ['Mary']]]])
        NB: In this list-based tree notation, non-initial list elements are subtrees,
             and so must be lists.
    """
    if isinstance(t, list) and len(t)>0:
        print('%s %s' % (n*' ', str(t[0]))) # print root
        for subtree in t[1:]:  # then subtrees indented by 4
            pptree(n+4, subtree)
    else:
        raise RuntimeError('pptree error')

if __name__ == '__main__':
    """ examples """
    pptree(0, ['TP', ['DP', ['John']], ['VP', ['V',['praises']], ['DP', ['Mary']]]])
    # pptree(0, [1, 2, [3, 4], [5, 6]])   # not well formed!
    pptree(0, [1, [2], [3, [4]], [5, [6]]])
