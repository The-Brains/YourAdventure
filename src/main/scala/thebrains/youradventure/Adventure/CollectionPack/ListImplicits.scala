package thebrains.youradventure.Adventure.CollectionPack

import scalaz.Maybe
import thebrains.youradventure.Utils.Err
import scalaz.zio.IO

object ListImplicits {
  implicit final class ListSimpleTransformations[A](l: List[A]) {

    /**
      * Get list of item which are extra to be distinct, distinct
      *
      * @return
      */
    def getExtrasDistinct: List[A] = {
      getExtras.distinct
    }

    def isUnique: Boolean = {
      l.distinct.length == l.length
    }

    /**
      * Get list of item which are extra to be distinct
      *
      * @return
      */
    def getExtras: List[A] = {
      l.diff(l.distinct)
    }

    def safeReduce(f: (A, A) => A)(orElse: A): A = {
      if (l.isEmpty) {
        orElse
      } else {
        l.reduce(f)
      }
    }
  }

  implicit final class ListOutTransformations[+A](l: List[A]) {
    def filterMap[B >: A](p: A => Boolean)(update: A => B): List[B] = {
      l.map(a => if (p(a)) update(a) else a)
    }

    def updateFirst[B >: A](p: A => Boolean)(update: A => B): List[B] = {
      val found = l.zipWithIndex.find { case (item, _) => p(item) }
      found match {
        case Some((item, idx)) => l.updated(idx, update(item))
        case None              => l
      }
    }

    def updateFirstIO[B >: A](p: A => Boolean)(update: A => IO[Err, B]): List[IO[Err, B]] = {
      val found = l.zipWithIndex.find { case (item, _) => p(item) }
      found match {
        case Some((item, idx)) =>
          l.map(a => IO.fromEither[Err, B](Right(a)))
            .updated(idx, update(item))
        case None => l.map(a => IO.fromEither[Err, B](Right(a)))
      }
    }
  }

  implicit class OptionsMaybeMaybes[A](l: List[Maybe[A]]) {
    def headMaybe: Maybe[A] = Maybe.fromOption(l.headOption).flatMap(identity)
  }

  implicit class OptionsMaybes[A](l: List[A]) {
    def headMaybe: Maybe[A] = Maybe.fromOption(l.headOption)
  }
}
