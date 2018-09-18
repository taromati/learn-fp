package learnfp.functor

object ListInstance {
  implicit val listInstance:Functor[List] = new Functor[List] {
  //    override def fmap[A, B](a: List[A])(fx: A => B): List[B] = {
  //      @annotation.tailrec
  //      def step(acc: List[B], remains: List[A]): List[B] =
  //        remains match {
  //          case Nil => acc.reverse
  //          case x :: xs => step(fx(x) :: acc, xs)
  //        }
  //      step(Nil, a)
  //    }
    override def fmap[A, B](a: List[A])(fx: A => B): List[B] = a.map(aa => fx(aa))
  }
}
