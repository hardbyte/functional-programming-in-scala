
// Kenni's answers
// https://github.com/kennib/FP-exercises


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object tree {

  // Size of tree
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Max value in tree
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  // Map f on every element
  def map[A, B](t: Tree[A], f:(A => B)): Tree[B] = t match {
    case Leaf(value) => Leaf( f (value) )
    case Branch(left, right) => Branch(map(left, f), map(right, f))
  }


  // generalize with "fold"
  // Like `foldRight` for lists, `fold` receives a
  // "handler" for each of the data constructors of the type,
  // and recursively accumulates some value using these
  // handlers. As with `foldRight`,
  // `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
  // this function to implement just about any recursive function
  // that would otherwise be defined by pattern matching.
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(l, r) => g( fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizef[A](t: Tree[A]): Int = {
    fold(t)(x => 1)((l, r) => 1 + l + r)
  }

  def maximumf(t: Tree[Int]): Int = fold(t)(x => x)((l, r) => l max r)

  def depthf(t: Tree[Any]): Int = fold(t)(x => 1)((l, r) => 1 + (l max r))

  def mapf[A, B](t: Tree[A], f:(A => B)): Tree[B] = {
    fold(t)(x => Leaf(f(x)):Tree[B])((l, r) => Branch(l, r))
  }
}

