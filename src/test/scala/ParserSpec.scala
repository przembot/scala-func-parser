package com.przembot.test

import org.scalatest.FunSuite
import com.przembot.Widget

class WidgetTest extends FunSuite {

  test("colour") {
    assert("Blue" === new Widget().colour)
  }

  test("disposition") {
    assert("Awesome" === new Widget().disposition)
  }

}
