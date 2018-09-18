package learnfp.foldable

import learnfp.functor.Disjunction.{Disjunction, LeftDisjunction, RightDisjunction}
import learnfp.functor.Id
import learnfp.functor.Maybe.{Just, Maybe, Nothing}

trait Foldable[C[_]] {
  def foldr[A, B](xs:C[A])(init:B)(fx:(A, B) => B):B
}

object FoldableInstances {
  implicit def idFoldable = new Foldable[Id] {
    override def foldr[A, B](xs: Id[A])(init: B)(fx: (A, B) => B): B = fx(xs.value, init)
  }

  implicit def listFoldable = new Foldable[List] {
    override def foldr[A, B](xs: List[A])(init: B)(fx: (A, B) => B): B = {
      @annotation.tailrec
      def step(b: B, remains: List[A]): B =
        remains match {
          case Nil => b
          case xss :: xsss => step(fx(xss, b), xsss)
        }
      step(init, xs.reverse)

//      xs match {
//        case h :: t => fx(h, foldr(t)(init)(fx))
//        case Nil => init
//      }
    }
  }

  implicit def tuple2Foldable = new Foldable[({type E[X] = Tuple2[X, X]})#E] {
    override def foldr[A, B](xs: (A, A))(init: B)(fx: (A, B) => B): B = {
      val (a1, a2) = xs
      fx(a1, fx(a2, init))
    }
  }

  implicit def tuple3Foldable = new Foldable[({type E[X] = (X, X, X)})#E] {
    override def foldr[A, B](xs: (A, A, A))(init: B)(fx: (A, B) => B): B = {
      val (a1, a2, a3) = xs
      fx(a1, fx(a2, fx(a3, init)))
    }
  }

  implicit val maybeFoldable = new Foldable[Maybe] {
    override def foldr[A, B](xs: Maybe[A])(init: B)(fx: (A, B) => B): B = xs match {
      case Just(v) => fx(v, init)
      case Nothing() => init
    }
  }

  implicit def disjunctionFoldable[L] = new Foldable[({type E[X] = Disjunction[L, X]})#E] {
    override def foldr[A, B](xs: Disjunction[L, A])(init: B)(fx: (A, B) => B): B = xs match {
      case LeftDisjunction(v) => init
      case RightDisjunction(v) => fx(v, init)
    }
  }
}

class FoldableOps[C[_], A](xs:C[A])(implicit foldable: Foldable[C]) {
  def myfoldr[B](init:B)(fx:(A, B) => B):B = foldable.foldr(xs)(init)(fx)
}

object FoldableOps {
  implicit def toFoldableOps[C[_], A](xs:C[A])(implicit foldable: Foldable[C]) = new FoldableOps[C, A](xs)
  implicit def tuple2ToFoldableOps[A](xs:(A, A))(implicit foldable:Foldable[({type E[X] = (X, X)})#E]) = new FoldableOps[({type E[X] = (X, X)})#E, A](xs)
  implicit def tuple3ToFoldableOps[A](xs:(A, A, A))(implicit foldable:Foldable[({type E[X] = (X, X, X)})#E]) = new FoldableOps[({type E[X] = (X, X, X)})#E, A](xs)
  implicit def disjunctionToFoldableOps[L, A](xs:Disjunction[L, A])(implicit foldable:Foldable[({type E[X] = Disjunction[L, X]})#E]) =
    new FoldableOps[({type E[X] = Disjunction[L, X]})#E, A](xs)
}
