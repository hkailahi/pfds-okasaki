package bookclub

object Chapter8 {
    def main(args: Array[String]): Unit = {
        val rs = RotationState(List(1, 2), List(5, 4, 3))
        val rsDone = (0 until 8).foldLeft(rs) { (rs, _) => println(rs); rs.exec }
        println()

        // remove the first three elements:
        val rsInv = (0 until 3).foldLeft(rs) { (rs, _) => println(rs); rs.exec.invalidate }
        println(rsInv.exec)
        println()

        run[HMQueue]
        run[HMDiffQueue]
    }

    def run[Q[_]](implicit Q: Queue[Q]): Unit = {
        println(s"\n${Q}\n")

        implicit class QOps[T](q: Q[T]) {
            def snoc(x: T): Q[T] = {
                println(s"snoc(${x})")
                val q0 = Q.snoc(q, x)
                println(Q.repr(q0))
                println()
                q0
            }
            def tail: Q[T] = {
                println("tail")
                val q0 = Q.tail(q)
                println(Q.repr(q0))
                println()
                q0
            }
        }

        val q0 = Q.empty[Int]

        // append 0 .. 6:
        val q6 = (0 until 7).foldLeft(q0) { (q, x) => q.snoc(x) }
        println()

        // remove 0, 1; append 7, 8
        val qp = q6.tail.snoc(7).tail.snoc(8)
    }
}

object DumbImplicits {
    // Ugh. Where would this normally come from?
    @SuppressWarnings(Array("org.wartremover.warts.Equals"))
    implicit final class AnyOps[A](self: A) {
       def ===(other: A): Boolean = self == other
    }
}

trait Queue[Q[_]] {
    def empty[T]: Q[T]
    def isEmpty[T](queue: Q[T]): Boolean
    def snoc[T](queue: Q[T], x: T): Q[T]
    def head[T](queue: Q[T]): T
    def tail[T](queue: Q[T]): Q[T]

    def repr[T](queue: Q[T]): String
}


sealed trait RotationState[T] {
    import RotationState._

    @SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.ToString"))
    def exec: RotationState[T] = this match {
        case s@Idle() => s

        case Reversing(ok, x :: f, fp, y :: r,   rp) => Reversing(ok+1, f, x :: fp, r, y :: rp)
        case Reversing(ok, Nil,    fp, y :: Nil, rp) => Appending(ok, fp, y :: rp)

        case Appending(0, _, rp)      => Done(rp)
        case Appending(ok, x :: fp, rp) => Appending(ok-1, fp, x :: rp)

        case s@Done(_) => s

        case other => throw new Exception("unexpected: " + other.toString)
    }

    /** Mark one element from f as removed. */
    @SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.ToString"))
    def invalidate: RotationState[T] = this match {
        case s@Idle() => s
        case Reversing(ok, f, fp, r, rp) => Reversing(ok-1, f, fp, r, rp)
        case Appending(0, fp, x :: rp) => Appending(0, fp, rp)
        case Appending(ok, fp, rp) => Appending(ok-1, fp, rp)
        // case s@Done(_) => s

        case other => throw new Exception("unexpected: " + other.toString)
   }
}
object RotationState {
    // ok is the number of elements from the head of fp that will be copied when Appending

    final case class Idle[T]() extends RotationState[T]
    final case class Reversing[T](ok: Int, f: List[T], fp: List[T], r: List[T], rp: List[T]) extends RotationState[T]
    final case class Appending[T](ok: Int, fp: List[T], rp: List[T]) extends RotationState[T]
    final case class Done[T](f: List[T]) extends RotationState[T]

    def apply[T](f: List[T], r: List[T]): RotationState[T] =
        Reversing(0, f, Nil, r, Nil)
}


final case class HMQueue[T](lenf: Int, f: List[T], state: RotationState[T], lenr: Int, r: List[T]) {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion", "org.wartremover.warts.ToString"))
    def repr: String = {
        import RotationState._

        def contents(s: RotationState[T]): List[T] = s match {
            case Idle() => f ++ r.reverse

            case Done(newf) => newf

            case other => contents(other.exec)
        }

        s"${contents(state).mkString(", ")}; { ${f}; ${state}; ${r} }"
    }
}
object HMQueue {
    import DumbImplicits._
    import RotationState._

    @SuppressWarnings(Array("org.wartremover.warts.Equal", "org.wartremover.warts.ToString"))
    implicit val HMQueueQueue: Queue[HMQueue] = new Queue[HMQueue] {
        def empty[T]: bookclub.HMQueue[T] = HMQueue(0, Nil, Idle(), 0, Nil)
        def isEmpty[T](queue: bookclub.HMQueue[T]): Boolean = queue.lenf === 0

        def snoc[T](q: bookclub.HMQueue[T], x: T): bookclub.HMQueue[T] =
            check(HMQueue(q.lenf, q.f, q.state, q.lenr+1, x :: q.r))

        @SuppressWarnings(Array("org.wartremover.warts.Throw"))
        def head[T](q: bookclub.HMQueue[T]): T =
            q.f match {
                case Nil => throw new Exception("head: " + q.toString)
                case x :: _ => x
            }

        @SuppressWarnings(Array("org.wartremover.warts.Throw"))
        def tail[T](q: bookclub.HMQueue[T]): bookclub.HMQueue[T] =
            q.f match {
                case Nil => throw new Exception("tail: " + q.toString)
                case x :: f => check(HMQueue(q.lenf-1, f, q.state.invalidate, q.lenr, q.r))
            }

        def exec2[T](q: HMQueue[T]): HMQueue[T] = {
            println("  executing: " + q.repr)
            val (newf, newstate) = q.state.exec.exec match {
                case Done(newf) => (newf, Idle[T]())
                case s          => (q.f, s)
            }
            HMQueue(q.lenf, newf, newstate, q.lenr, q.r)
        }

        def check[T](q: HMQueue[T]): HMQueue[T] =
            if (q.lenr <= q.lenf) exec2(q)
            else {
                println("  rotating: " + q.repr)
                val newstate = RotationState(q.f, q.r)
                exec2(HMQueue(q.lenf + q.lenr, q.f, newstate, 0, Nil))
            }

        def repr[T](q: HMQueue[T]) = q.repr
    }
}




final case class HMDiffQueue[T](diff: Int, f: List[T], state: RotationState[T], r: List[T]) {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion", "org.wartremover.warts.ToString"))
    def repr: String = {
        import RotationState._

        def contents(s: RotationState[T]): List[T] = s match {
            case Idle() => f ++ r.reverse

            case Done(newf) => newf

            case other => contents(other.exec)
        }

        s"${contents(state).mkString(", ")}; { ${diff}; ${f}; ${state}; ${r} }"
    }
}
object HMDiffQueue {
    import DumbImplicits._
    import RotationState._

    @SuppressWarnings(Array("org.wartremover.warts.Equal", "org.wartremover.warts.ToString"))
    implicit val HMDiffQueueQueue: Queue[HMDiffQueue] = new Queue[HMDiffQueue] {
        def empty[T]: HMDiffQueue[T] = HMDiffQueue(0, Nil, Idle(), Nil)
        def isEmpty[T](queue: HMDiffQueue[T]): Boolean = queue.diff === 0 && queue.f === Nil

        def snoc[T](q: bookclub.HMDiffQueue[T], x: T): HMDiffQueue[T] =
            check(HMDiffQueue(q.diff-1, q.f, q.state, x :: q.r))

        @SuppressWarnings(Array("org.wartremover.warts.Throw"))
        def head[T](q: HMDiffQueue[T]): T =
            q.f match {
                case Nil => throw new Exception("head: " + q.toString)
                case x :: _ => x
            }

        @SuppressWarnings(Array("org.wartremover.warts.Throw"))
        def tail[T](q: HMDiffQueue[T]): HMDiffQueue[T] =
            q.f match {
                case Nil => throw new Exception("tail: " + q.toString)
                case x :: f => check(HMDiffQueue(q.diff-1, f, q.state.invalidate, q.r))
            }

        def exec2[T](q: HMDiffQueue[T]): HMDiffQueue[T] = {
            println("  executing: " + q.repr)
            val (newf, newstate) = q.state.exec.exec match {
                case Done(newf) => (newf, Idle[T]())    // TODO: get new diff from this state?
                case s          => (q.f, s)             // diff here is q.diff
            }
            val newdiff = newf.size - q.r.size  // TODO: not OK!
            // q.diff = old f size - r.size
            // need to keep track of number of invalidates?
            HMDiffQueue(newdiff, newf, newstate, q.r)
        }

        def check[T](q: HMDiffQueue[T]): HMDiffQueue[T] =
            if (q.diff >= 0) exec2(q)
            else {
                println("  rotating: " + q.repr)
                val newstate = RotationState(q.f, q.r)
                exec2(HMDiffQueue(q.diff, q.f, newstate, Nil))
            }

        def repr[T](q: HMDiffQueue[T]) = q.repr
    }
}
