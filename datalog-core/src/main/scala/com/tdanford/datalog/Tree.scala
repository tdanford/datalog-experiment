/*
 * Copyright 2014 Timothy Danford
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.tdanford.datalog

trait Tree[T] {
  def label : T
  def contains( path : Seq[T] ) : Boolean
  def traverse( path : Seq[T] ) : Option[(Tree[T], Seq[T])]
  def insert( path : Seq[T] ) : Tree[T]

  def search( path : Seq[T] ) : Tree[T] = traverse(path).map(_._1).orNull
  def insertAll( paths : Seq[T]* ) : Tree[T] = paths.foldLeft(this)( (t, path) => t.insert(path) )
}

case class Root[T]( children : Seq[Tree[T]] = Seq() ) extends Tree[T] {
  override def label: T = null.asInstanceOf[T]

  override def insert(path: Seq[T]): Tree[T] =
    path.headOption.map {
      case head : T =>
        val updatedChild = children.find( _.label == head ).map(_.insert(path.tail)).getOrElse {
          if (path.tail.length == 0) Leaf[T](head) else Branch[T](head, Seq()).insert(path.tail)
        }
        Root[T](children.filter(_.label != head) :+ updatedChild)
    }.getOrElse(this)

  override def traverse(path: Seq[T]): Option[(Tree[T], Seq[T])] = children.map(_.traverse(path)).head

  override def contains(path: Seq[T]): Boolean = children.exists(_.contains(path))
}

case class Leaf[T]( label : T ) extends Tree[T] {

  override def contains(path: Seq[T]): Boolean =
    path == Seq(label)

  override def traverse(path: Seq[T]): Option[(Tree[T], Seq[T])] =
    if(contains(path)) Some((this, Nil)) else None

  override def insert(path: Seq[T]): Tree[T] =
    path.headOption.map {
      case pathHead : T => Branch[T](label, Seq()).insert(path)
    }.getOrElse(this)
}

case class Branch[T]( label : T, children : Seq[Tree[T]] ) extends Tree[T] {

  override def contains(path: Seq[T]): Boolean =
    path.headOption.exists(pt => pt == label && children.exists(_.contains(path.tail)))

  override def traverse(path: Seq[T]): Option[(Tree[T], Seq[T])] =
    path.headOption.map {
      case head: T =>
        if (label == head) {
          children.flatMap(_.traverse(path.tail)).headOption
        } else {
          None
        }
    }.getOrElse(Some(this -> path))

  override def insert(path: Seq[T]): Tree[T] =
    path.headOption.map {
      case head : T =>
        Branch[T](label, children.map {
          case child : Tree[T] =>
            if(child.label == head)
              child.insert(path.tail)
            else
              child
        })
    }.getOrElse(this)

}

object Pattern {
  def termPattern( t : Term ) : Pattern[Term] =
    t match {
      case Var(name) => Wildcard[Term]()
      case Constant(name) => Exact[Term](t)
      case Func(fname, terms) => new FuncPattern(fname, terms)
      case _ => Exact[Term](null)
    }
}

trait Pattern[T] {
  def matches( t : T ) : Boolean
}

class FuncPattern(fname : String, terms : Seq[Term]) extends Pattern[Term] {
  override def matches(fterm: Term): Boolean = fterm match {
    case Func(pfname, pterms) => fname == pfname &&
      terms.map(Pattern.termPattern).zip(pterms).forall {
        case (patt, pt) => patt.matches(pt)
      }
    case _ => false
  }
}

case class FunctionalPattern[T,U]( f : T=>U, subPattern : Pattern[U] ) extends Pattern[T] {
  override def matches(t: T): Boolean = subPattern.matches(f(t))
}

case class Exact[T]( value : T ) extends Pattern[T] {
  override def matches(t: T): Boolean = t == value
}

case class Wildcard[T]() extends Pattern[T] {
  override def matches(t: T): Boolean = true
}

