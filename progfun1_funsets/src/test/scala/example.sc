import funsets.FunSets._


printSet(singletonSet(1))
printSet(singletonSet(1))

printSet(union(singletonSet(1), singletonSet(2)))
printSet(union(singletonSet(1), singletonSet(2)))

printSet(intersect(singletonSet(1), singletonSet(1)))
printSet(intersect(singletonSet(1), singletonSet(2)))
printSet(intersect(union(singletonSet(1), singletonSet(2)), union(singletonSet(2), singletonSet(3))))

printSet(diff(singletonSet(1), union(singletonSet(1), singletonSet(2))))
printSet(diff(union(singletonSet(1), singletonSet(2)), singletonSet(1)))
printSet(diff(
  union(union(union(union(union(singletonSet(1), singletonSet(3)), singletonSet(4)), singletonSet(5)), singletonSet(7)), singletonSet(1000)),
  union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4))
))
printSet(diff(
  union(union(union(singletonSet(1), singletonSet(2)), singletonSet(3)), singletonSet(4)),
  union(singletonSet(-1000), singletonSet(0))
))


contains(filter(union(singletonSet(1), singletonSet(2)), x => x > 0), 1)
contains(filter(union(singletonSet(1), singletonSet(2)), x => x > 0), 2)
contains(filter(union(singletonSet(1), singletonSet(2)), x => x > 1), 1)
contains(filter(union(singletonSet(1), singletonSet(2)), x => x > 1), 2)

forall(union(singletonSet(5), singletonSet(2)), x => x % 2 == 0)
forall(union(singletonSet(4), singletonSet(2)), x => x % 2 == 0)

exists(union(singletonSet(5), singletonSet(2)), x => x % 2 == 0)
exists(union(singletonSet(5), singletonSet(3)), x => x % 2 == 0)

contains(map(union(singletonSet(5), singletonSet(2)), x=>2*x), 2)
contains(map(union(singletonSet(5), singletonSet(2)), x=>2*x), 5)
contains(map(union(singletonSet(5), singletonSet(2)), x=>2*x), 4)
contains(map(union(singletonSet(5), singletonSet(2)), x=>2*x), 10)