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

class LiteralSuite extends FunSuite {

  test("equality between Literals holds in the correct way") {
    assert( Literal("foo", Seq(Var("a"), Constant("b"))) === Literal("foo", Seq(Var("a"), Constant("b"))) )
  }

  test("isGround is determined correctly") {
    assert(!Literal("foo", Seq(Var("a"), Constant("b"))).isGround )
    assert(Literal("foo", Seq(Constant("a"), Constant("b"))).isGround )
    assert(Literal("foo", Seq(Func("f", Seq(Constant("a"))), Constant("b"))).isGround )
    assert(!Literal("foo", Seq(Func("f", Seq(Var("a"))), Constant("b"))).isGround )
  }

}
