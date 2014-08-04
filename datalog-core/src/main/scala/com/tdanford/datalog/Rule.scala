/**
 * Copyright 2014 Timothy Danford
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.tdanford.datalog

class RuleSet( rules : Seq[Rule] ) {
}

case class Rule( head : Literal, body : Seq[Literal] ) {
  require( !head.negated, "Head literal in a Rule cannot be negated" )
  override def toString : String = "%s :- %s".format(head.toString, body.map(_.toString).mkString(",\n"))
}

case class Literal(predicate : String, terms : Seq[Term], negated : Boolean = false) {
  override def toString: String = {
    val pstr = "%s(%s)".format(predicate, terms.map(_.toString).mkString(","))
    if(negated) "!%s".format(pstr) else pstr
  }

  def isGround : Boolean = terms.forall(_.isGround)
}

object Literal {
  def fact(predicate : String, values : String*) : Literal =
    Literal(predicate, values.map(str => Constant(str)))
}

trait Term {
  def isGround : Boolean
}

case class Var( name : String ) extends Term {
  override def toString: String = name
  def isGround : Boolean = false
}

case class Constant( value : String ) extends Term {
  def isGround : Boolean = true
  override def toString: String = value
}

case class Func( fname : String, args : Seq[Term] ) extends Term {
  def isGround : Boolean = args.forall(_.isGround)
  override def toString : String = "%s(%s)".format(fname, args.map(_.toString).mkString(","))
}

