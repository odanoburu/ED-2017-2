'''
Depth-first search
'''

#Input: Grafo
#Output: Se o grafo foi visitado, pre e pos visit e clock (contador)
def DFS(query_node, parents):
    # Acompanha todos os nós visitados
    result = {}
    # Acompanha os nós a serem verificados
    stack = []
    stack.append( (query_node, 0) )
    while len(stack) > 0:
        print("stack=", stack)
        node, dist = stack.pop()
        result[node] = dist
        if node in parents:
            for parent in parents[node]:
                stack_members = [x[0] for x in stack]
                if parent not in stack_members:
                    stack.append( (parent, dist+1) )
    return result


if __name__ == "__main__":

    parents = dict()
    parents = {'N1': ['N2', 'N3', 'N4'], 'N3': ['N6', 'N7'], 'N4': ['N3'], 'N5': ['N4', 'N8'], 'N6': ['N13'],
               'N8': ['N9'], 'N9': ['N11'], 'N10': ['N7', 'N9'], 'N11': ['N14'], 'N12': ['N5']}

    print("DFS:")
    dist = DFS('N1', parents)
    print(dist)
