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

import org.scalatest.FunSuite

class TreeSuite extends FunSuite {

  test("adding a Seq of length 1 to a Root node adds one Leaf child underneath it") {
    val t1 = Root[String]()
    val t2 = t1.insert(Seq("a"))

    assert(t2.isInstanceOf[Root[String]])

    val root = t2.asInstanceOf[Root[String]]

    assert( root.children.size === 1 )
    assert( root.children(0).isInstanceOf[Leaf[String]] )
  }
}
