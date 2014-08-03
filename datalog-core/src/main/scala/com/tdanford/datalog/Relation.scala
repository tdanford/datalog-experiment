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

import java.io._

import scala.io.Source

trait Relation {
  def name : String
  def scan() : Iterator[Predicate]
}

object PredicateConditions {

  def predicateSubset( predicate : Predicate, indices : Int* ) : Seq[Atom] =
    predicate.tuple.zipWithIndex.filter {
      case (a : Atom, i : Int) => indices.contains(i)
    }.map(_._1)

  def pointEquality( indices : Int* ) : (Predicate,Predicate)=>Boolean =
    (p1, p2) => {
      val s1: Seq[Atom] = predicateSubset(p1, indices : _*)
      val s2: Seq[Atom] = predicateSubset(p2, indices : _*)
      s1 == s2
    }
}

case class LocalUnion( name : String, first : Relation, second : Relation ) extends Relation {
  override def scan(): Iterator[Predicate] = first.scan() ++ second.scan()
}

case class LocalJoin( name : String,
                      joinCondition : (Predicate, Predicate) => Boolean,
                      left : Relation,
                      right : Relation) extends Relation {

  override def scan(): Iterator[Predicate] =
    left.scan().flatMap {
      case leftPred : Predicate =>
        right.scan().filter( rightPred => joinCondition(leftPred, rightPred) )
    }
}

object TreeUtils {
  def buildLiteralTree( literals : Seq[Literal] ) : Tree[Term] =
    Root[Term]().insertAll(literals.map(_.terms) : _*)
}

case class InMemory( name : String, values : Seq[Literal] ) extends Relation {

  private lazy val index : Tree[Term] = TreeUtils.buildLiteralTree(values)

  def this( rel : Relation ) = this( rel.name, rel.scan().toSeq )

  def ++(mem : InMemory) : InMemory = InMemory(name, Set(values ++ mem.values : _*).toSeq)

  override def scan(): Iterator[Literal] = values.iterator
}

case class FromFile( name : String, file : File ) extends Relation {

  import Literal._

  override def scan() : Iterator[Literal] = {
    val lines = Source.fromFile(file, "UTF-8").getLines()
    lines.map(lineToPredicate)
  }

  def lineToPredicate( line : String ) : Predicate = {
    val array = line.split("\t")
    Predicate(name, array.map(str => Value(str)))
  }
}
