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


case class Rule( head : Predicate, body : Seq[Literal] ) {
  override def toString : String = "%s :- %s".format(head.toString, body.map(_.toString).mkString(",\n"))
}

case class Literal(relation : Predicate, negated : Boolean = false) {
  override def toString: String =
    if(negated) "!%s".format(relation.toString) else relation.toString
}

trait Atom {
}

case class Var( name : String ) extends Atom {
  override def toString: String = name
}

case class Value( value : String ) extends Atom {
  override def toString: String = value
}

case class Func( fname : String, args : Seq[Atom] ) extends Atom {
  override def toString : String = "%s(%s)".format(fname, args.map(_.toString).mkString(","))
}

case class Predicate( name : String, tuple : Seq[Atom] ) {
  override def toString : String = "%s(%s)".format(name, tuple.map(_.toString).mkString(","))
}
