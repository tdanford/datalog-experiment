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
  def scan() : Iterator[Literal]
}

object LiteralConditions {

  def predicateSubset( predicate : Literal, indices : Int* ) : Seq[Term] =
    predicate.terms.zipWithIndex.filter {
      case (a : Term, i : Int) => indices.contains(i)
    }.map(_._1)

  def pointEquality( indices : Int* ) : (Literal,Literal)=>Boolean =
    (p1, p2) => {
      val s1: Seq[Term] = predicateSubset(p1, indices : _*)
      val s2: Seq[Term] = predicateSubset(p2, indices : _*)
      s1 == s2
    }
}

case class LocalUnion( name : String, first : Relation, second : Relation ) extends Relation {
  override def scan(): Iterator[Literal] = first.scan() ++ second.scan()
}

case class LocalJoin( name : String,
                      joinCondition : (Literal, Literal) => Boolean,
                      left : Relation,
                      right : Relation) extends Relation {

  override def scan(): Iterator[Literal] =
    left.scan().flatMap {
      case leftPred : Literal =>
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
    lines.map(lineToLiteral)
  }

  def lineToLiteral( line : String ) : Literal = fact(name, line.split("\t") : _*)
}
