// 'summon' a Functor instance for List...
/*implicitly[Functor[List]]
res0.map(List(1, 2, 3))(_ + 1)

implicitly[Functor[Int => ?]]
res2.map(_ + 1)(_ + 2) // res3: Int => Int
res2(5) // 8

Functor[List] compose Functor[Option]
val xs: List[Option[Int]] = List(Some(1), Some(2), Some(3))
res3.map(xs)(_ + 1) // List(Some(2), Some(3), Some(4))*/ 