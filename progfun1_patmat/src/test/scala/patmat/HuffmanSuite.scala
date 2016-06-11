package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Leaf('d',4), Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("string2chars(\"\")") {
    assert(string2Chars("") === List())
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("makeOrderedLeafList for no frequencies") {
    assert(makeOrderedLeafList(List()) === List())
  }


  test("makeCodeTree for left-right") {
    assert(makeCodeTree(Leaf('e', 1), Leaf('t', 2)) === Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3))
  }


  test("makeCodeTree for right-left") {
    assert(makeCodeTree(Leaf('e', 2), Leaf('t', 1)) === Fork(Leaf('t',1),Leaf('e',2),List('e', 't'),3))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("combine of none leaf list") {
    val leaflist = List()
    assert(combine(leaflist) === List())
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }


  test("decode and encode an empty text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("".toList)) === "".toList)
    }
  }


  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }


  test("decode and quickEncode an empty text should be identit") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("".toList)) === "".toList)
    }
  }


  test("HuffmanSuite::singleton of list size 3") {
    new TestTrees {
      assert(singleton(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))) === false)
    }
  }


  test("HuffmanSuite::singleton of singleton") {
    new TestTrees {
      assert(singleton(List(Leaf('e', 1))) === true)
    }
  }


  test("HuffmanSuite::singleton of nil") {
    new TestTrees {
      assert(singleton(Nil) === false)
    }
  }


  test("'createCodeTree(babab) equals t1'") {
    new TestTrees {
      val codeTree = createCodeTree("babab".toList)
      assert(codeTree === t1)
    }
  }


  test("'createCodeTree(aabbb) equals t1'") {
    new TestTrees {
      val codeTree = createCodeTree("aabbb".toList)
      assert(codeTree === t1)
    }
  }


  test("'createCodeTree(aabbbdddd) equals t2'") {
    new TestTrees {
      val codeTree = createCodeTree("aabbbdddd".toList)
      assert(codeTree === t2)
    }
  }

}
