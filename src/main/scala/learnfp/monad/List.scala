package learnfp.monad

import learnfp.functor.ListInstance._

object ListInstance {
  implicit val listMonadInstance = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def flatMap[A, B](a: List[A])(fx: A => List[B]): List[B] = {
      @annotation.tailrec
      def step(acc: List[B], remains: List[A]): List[B] =
        remains match {
          case Nil => acc.reverse
          case x :: xs => step(fx(x).reverse ::: acc, xs)
        }
      step(Nil, a)
    }
  }
}
