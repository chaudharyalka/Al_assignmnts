import numpy
import itertools

# dfs of a tree:
def dfs(tree):
 length = len(tree)

 if (not(length == 0)):
   if ((length) == 1):
      return ([tree[0]])
   else:
     return ([tree[0]] + list (numpy.concatenate(map(dfs,tree[1:]))))
 else:
   return []

def dfs2(tree):
 length = len(tree)

 if (not(length == 0)):
   if ((length) == 1):
      return ([tree[0]])
   else:
     return ([tree[0]] + [x for y in map(dfs,tree[1:]) for x in y] )
 else:
   return []


def bfs(tree):
  length = len(tree)
  if (length == 0):
    return []
  else:
    return (map(root,tree) + (bfs(list(itertools.chain.from_iterable(map(children,tree))) )))

def bfs2(tree):
  length = len(tree)
  if (length == 0):
    return []
  else:
    return (map(root,tree) + (bfs([x for y in (map(children,tree)) for x in y]) ))




def root(tree):
   return (tree[0]) 
def children(tree):
   return (tree[1:])

tree = [5,[6,[7],[8,[3],[4],[56],[12],[45],[87]]],[9,[4],[2]]]
print bfs([tree])
print bfs2([tree])


 
