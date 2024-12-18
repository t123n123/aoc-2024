import sys

letters = []
lettermap = {}
parent = {}

perim = {}
area = {}


def getparent(p):
    if p not in parent:
        parent[p] = p
        perim[p] = 4

    if parent[p] != parent[parent[p]]:
        parent[p] = getparent(parent[p])

    return parent[p]


def unite(p1, p2):
    if p1 not in parent:
        parent[p1] = p1
        perim[p1] = 4
    if p2 not in parent:
        parent[p2] = p2
        perim[p2] = 4

    p1 = getparent(p1)
    p2 = getparent(p2)

    if p1 != p2:
        parent[p2] = p1
        perim[p1] += perim[p2]

    perim[p1] -= 2


for line in sys.stdin:
    letters.append(line.strip())

for i in range(len(letters)):
    for j in range(len(letters[i])):
        lettermap[(i, j)] = letters[i][j]
        if i > 0 and letters[i - 1][j] == letters[i][j]:
            unite((i - 1, j), (i, j))
        if j > 0 and letters[i][j - 1] == letters[i][j]:
            unite((i, j - 1), (i, j))

# part 1

for i in range(len(letters)):
    for j in range(len(letters[i])):
        p = getparent((i, j))
        if p not in area:
            area[p] = 1
        else:
            area[p] += 1

res = 0

for i in range(len(letters)):
    for j in range(len(letters[i])):
        p = getparent((i, j))
        if p == (i, j):
            res += area[p] * perim[p]

print(res)

# part 2

corners = {}

for i in range(len(letters)):
    for j in range(len(letters[i])):
        p1 = (i, j)
        p = getparent(p1)
        if p not in corners:
            corners[p] = 0

        nb = [
            p2 not in lettermap or lettermap[p1] != lettermap[p2]
            for p2 in [
                (p1[0] + 1, p1[1]),
                (p1[0], p1[1] + 1),
                (p1[0] - 1, p1[1]),
                (p1[0], p1[1] - 1),
            ]
        ]

        if nb[0] and nb[1]:
            corners[p] += 1
        if nb[1] and nb[2]:
            corners[p] += 1
        if nb[2] and nb[3]:
            corners[p] += 1
        if nb[3] and nb[0]:
            corners[p] += 1

        for a, b, c in [
            ((p1[0] + 1, p1[1]), (p1[0], p1[1] + 1), (p1[0] + 1, p1[1] + 1)),
            ((p1[0] - 1, p1[1]), (p1[0], p1[1] - 1), (p1[0] - 1, p1[1] - 1)),
            ((p1[0] + 1, p1[1]), (p1[0], p1[1] - 1), (p1[0] + 1, p1[1] - 1)),
            ((p1[0] - 1, p1[1]), (p1[0], p1[1] + 1), (p1[0] - 1, p1[1] + 1)),
        ]:
            if (
                a in lettermap
                and b in lettermap
                and lettermap[a] == lettermap[p1]
                and lettermap[b] == lettermap[p1]
                and lettermap[c] != lettermap[p1]
            ):
                corners[p] += 1

res2 = 0
for i in range(len(letters)):
    for j in range(len(letters[i])):
        p = (i, j)
        if p == getparent(p):

            res2 += corners[p] * area[p]
print(res2)
