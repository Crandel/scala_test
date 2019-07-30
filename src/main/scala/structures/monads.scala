package structures

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (fmap(fab)(_._1), fmap(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => fmap(fa)(Left(_))
      case Right(fb) => fmap(fb)(Right(_))
    }
}

trait Monad[F[_]] {
  def mmap[A, B](fa: F[A])(f: A => B): F[B]

  def mflatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def lift[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    mflatMap(fa)(a => mmap(fb)(b => f(a, b)))
}


object MonadsExample {
  val listFunctor = new Functor[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }
}
