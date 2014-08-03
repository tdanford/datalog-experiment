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

import java.io.File

import org.scalatest.FunSuite

class FileRelationSuite extends FunSuite {

  test("correctly reads test_relation.txt") {
    val file = Thread.currentThread().getContextClassLoader.getResource("test_relation.txt").getFile
    val preds : Seq[Predicate] = new FileRelation("P", new File(file)).scan().toSeq

    assert( preds.size === 2 )
    assert( preds(0) === Predicate("P", Seq(Value("a"), Value("b"), Value("c"))) )
    assert( preds(1) === Predicate("P", Seq(Value("d"), Value("e"), Value("f"))) )
  }
}
