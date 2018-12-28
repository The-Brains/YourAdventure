package thebrains.youradventure.Adventure.CollectionPack

object ListImplicits {

  implicit class Uniqueness[A](l: List[A]) {
    def isUnique: Boolean = {
      l.distinct.length == l.length
    }

    def getExtras: List[A] = {
      l.diff(l.distinct).distinct
    }

    def safeReduce(f: (A, A) => A)(orElse: A): A = {
      if (l.isEmpty) {
        orElse
      } else {
        l.reduce(f)
      }
    }
  }

}
