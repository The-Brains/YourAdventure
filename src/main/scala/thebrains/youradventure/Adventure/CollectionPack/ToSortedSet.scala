package thebrains.youradventure.Adventure.CollectionPack

import scala.collection.immutable.SortedSet

object ToSortedSet {

  implicit class FromSet[A](collection: Set[A]) {
    def toSortedSet(implicit ord: Ordering[A]): SortedSet[A] = {
      SortedSet[A]() ++ collection
    }
  }

  implicit class FromList[A](collection: List[A]) {
    def toSortedSet(implicit ord: Ordering[A]): SortedSet[A] = {
      SortedSet[A]() ++ collection
    }
  }

}
