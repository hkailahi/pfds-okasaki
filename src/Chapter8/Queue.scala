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

    def run[Q[_]: Queue]: Unit = {
        import Queue._

        val q0 = empty[Q, Int]
        println(s"\n${q0.getClass.getName}\n")


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

/** Typeclass for Queue implementations. */
trait Queue[Q[_]] {
    def empty[T]: Q[T]
    def isEmpty[T](queue: Q[T]): Boolean
    def snoc[T](queue: Q[T], x: T): Q[T]
    def head[T](queue: Q[T]): T
    def tail[T](queue: Q[T]): Q[T]

    /** Print the queue, showing it's internal structure. */
    def repr[T](queue: Q[T]): String
}
object Queue {
    def empty[Q[_], T](implicit Q: Queue[Q]): Q[T] = Q.empty

    /** Postfix/infix syntax for Queue, which also does some tracing. */
    implicit class QueueOps[Q[_], T](q: Q[T])(implicit Q: Queue[Q]) {
        def isEmpty[T]: Boolean = Q.isEmpty(q)
        def snoc(x: T): Q[T] = {
            println(s"snoc(${x})")
            val q0 = Q.snoc(q, x)
            println(Q.repr(q0))
            println()
            q0
        }
        def head: T = Q.head(q)
        def tail: Q[T] = {
            println("tail")
            val q0 = Q.tail(q)
            println(Q.repr(q0))
            println()
            q0
        }
    }
}

sealed trait RotationState[T] {
    import RotationState._

    @SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.ToString"))
    def exec: RotationState[T] = execWithDiff._1

    /** Run one step, returning the new state and count of elements added to what will be
      * the new f and removed from the new r.
      * Note: that number turns out to 0 if the new state is Idle/Done, and 2 otherwise.
      */
    @SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.ToString"))
    def execWithDiff: (RotationState[T], Int) = this match {
        case s@Idle() => (s, 0)

        case Reversing(ok, x :: f, fp, y :: r,   rp) => (Reversing(ok+1, f, x :: fp, r, y :: rp), 2)  // moving y from r to (future) f
        case Reversing(ok, Nil,    fp, y :: Nil, rp) => (Appending(ok, fp, y :: rp), 2)  // moving y from r to (future) f

        case Appending(0, _, rp)        => (Done(rp), 0)
        case Appending(ok, x :: fp, rp) => (Appending(ok-1, fp, x :: rp), 0)

        case s@Done(_) => (s, 0)

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
        def isEmpty[T](queue: HMDiffQueue[T]): Boolean = queue.f === Nil

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
            val (s1, d1) = q.state.execWithDiff
            val (s2, d2) = s1.execWithDiff
            println(s"  and again: ${d1}; ${s1}; ${d2}")
            val (newf, newstate) = s2 match {
                case Done(newf) => (newf, Idle[T]())
                case s          => (q.f, s)
            }
            HMDiffQueue(q.diff + d1 + d2, newf, newstate, q.r)
        }

        def check[T](q: HMDiffQueue[T]): HMDiffQueue[T] =
            if (q.diff >= 0) exec2(q)
            else {
                // Note: q.diff == -1 and state == Idle
                println("  rotating: " + q.repr)
                val newstate = RotationState(q.f, q.r)
                exec2(HMDiffQueue(q.diff, q.f, newstate, Nil))
            }

        def repr[T](q: HMDiffQueue[T]) = q.repr
    }
}
