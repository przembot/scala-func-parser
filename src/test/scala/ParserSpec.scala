package com.przembot.test

import org.scalatest.FunSuite
import com.przembot.Parser
import com.przembot.ParserF._

class ParserTest extends FunSuite {

  test("parse single chars") {
    val parser = for (
      c <- char('c')
    ) yield c.toString()+"ala"

    assert(runParser(parser)("c") === Some("cala"))
  }

}
