package fhj.swengb.assignments.tree.rladstaetter

import javafx.scene.paint.Color

import org.junit.Assert._
import org.junit.Test

import scala.util.Try

/**
  * Test fixtures for the tree assignment
  */
class TreeAssignmentTest {

  import Graph._

  private val rootNode: Tree[L2D] = Node(L2D(Pt2D(0.0, 0.0), Pt2D(100, 0.0), colorMap(0)))

  private val treeOfSize1: Branch[L2D] = Branch(
    rootNode,
    Branch(
      Node(L2D(Pt2D(100.0, 0.0), Pt2D(199.985, -1.745), colorMap(0))),
      Node(L2D(Pt2D(100.0, 0.0), Pt2D(199.985, 1.745), colorMap(0)))))


  @Test def testRound(): Unit = {
    assertEquals(0.001, MathUtil.round(0.001232), 0.0)
    assertEquals(0.002, MathUtil.round(0.001532), 0.0)
    assertEquals(1.339, MathUtil.round(1.339), 0.0)
  }

  @Test def treeSize0(): Unit = {
    val t: Tree[L2D] = Graph.mkGraph(Pt2D(0, 0), 0, 100, 0, 1, 0)
    assertEquals(rootNode, t)
  }

  /**
    * tree is illegal since the depth is too high
    */
  @Test def illegalTree(): Unit = {
    assertTrue(Try(Graph.mkGraph(Pt2D(0, 0), 0, 100, 17, 1, 0)).isFailure)
  }

  @Test def treeSize1(): Unit = {
    val t: Tree[L2D] = Graph.mkGraph(Pt2D(0, 0), 0, 100, 1, 1, 1)
    assertEquals(
      treeOfSize1, t)
  }

  @Test def treeSize2(): Unit = {
    val t: Tree[L2D] = Graph.mkGraph(Pt2D(0, 0), 0, 100, 2, 1, 1)
    assertEquals(
      Branch(Node(L2D(Pt2D(0.0, 0.0), Pt2D(100.0, 0.0), colorMap(0))), Branch(Branch(Node(L2D(Pt2D(100.0, 0.0), Pt2D(199.985, -1.745), colorMap(0))), Branch(Node(L2D(Pt2D(199.985, -1.745), Pt2D(299.924, -5.235), colorMap(1))), Node(L2D(Pt2D(199.985, -1.745), Pt2D(299.985, -1.745), colorMap(1))))), Branch(Node(L2D(Pt2D(100.0, 0.0), Pt2D(199.985, 1.745), colorMap(0))), Branch(Node(L2D(Pt2D(199.985, 1.745), Pt2D(299.985, 1.745), colorMap(1))), Node(L2D(Pt2D(199.985, 1.745), Pt2D(299.924, 5.235), colorMap(1))))))), t)
  }

  @Test def testTraverse(): Unit = {
    val seqs: Seq[L2D] = Graph.traverse(treeOfSize1)(l2d => l2d)
    assertEquals(3, seqs.size)

    val expected =
      List(
        L2D(Pt2D(0.0, 0.0), Pt2D(100.0, 0.0), colorMap(0)),
        L2D(Pt2D(100.0, 0.0), Pt2D(199.985, -1.745), colorMap(0)),
        L2D(Pt2D(100.0, 0.0), Pt2D(199.985, 1.745), colorMap(0)))
    assertEquals(expected, seqs)

    expected.foreach {
      l2d => assertTrue(seqs.contains(l2d))
    }

  }

  // --- tests for the L2D class


  val epsilon: Double = 0.00000000001

  @Test def testLength(): Unit = {
    assertEquals(1.0, L2D(Pt2D(0, 0), Pt2D(0, 1), Color.GREEN).length, 0.000001)
    assertEquals(1.0, L2D(Pt2D(0, 0), Pt2D(0, -1), Color.GREEN).length, 0.000001)
    assertEquals(1.0, L2D(Pt2D(0, 0), Pt2D(1, 0), Color.GREEN).length, 0.000001)
    assertEquals(1.0, L2D(Pt2D(0, 0), Pt2D(-1, 0), Color.GREEN).length, 0.000001)
    assertEquals(1.4142135623730951, L2D(Pt2D(0, 0), Pt2D(1, 1), Color.GREEN).length, 0.000001)
  }


  /*
def areSomewhatEqual(a: L2D, b: L2D): Boolean = {
def areSomewhatEqual(a: Pt2D, b: Pt2D): Boolean = {
Math.abs(a.x - b.x) < epsilon && Math.abs(a.y - b.y) < epsilon
}
areSomewhatEqual(a.start, b.start) && areSomewhatEqual(a.end, b.end)
}      */

  @Test
  def testAngles(): Unit = {
    assertEquals(0, L2D(Pt2D(0, 0), Pt2D(1, 0), Color.GREEN).angle, epsilon)
    assertEquals(45, L2D(Pt2D(0, 0), Pt2D(1, 1), Color.GREEN).angle, epsilon)
    assertEquals(90, L2D(Pt2D(0, 0), Pt2D(0, 1), Color.GREEN).angle, epsilon)
    assertEquals(135, L2D(Pt2D(0, 0), Pt2D(-1, 1), Color.GREEN).angle, epsilon)
    assertEquals(180, L2D(Pt2D(0, 0), Pt2D(-1, 0), Color.GREEN).angle, epsilon)
    assertEquals(225, L2D(Pt2D(0, 0), Pt2D(-1, -1), Color.GREEN).angle, epsilon)
    assertEquals(270, L2D(Pt2D(0, 0), Pt2D(0, -1), Color.GREEN).angle, epsilon)
    assertEquals(315, L2D(Pt2D(0, 0), Pt2D(1, -1), Color.GREEN).angle, epsilon)

  }

  @Test def doesConstructorOfL2Dwork(): Unit = {
    val o = Pt2D(0, 0)
    assertEquals(L2D(o, Pt2D(1, 0), Color.GREEN), L2D(o, 0, 1, Color.GREEN))
    assertEquals(L2D(o, Pt2D(0, 1), Color.GREEN), L2D(o, 90, 1, Color.GREEN))
    assertEquals(L2D(o, Pt2D(-1, 0), Color.GREEN), L2D(o, 180, 1, Color.GREEN))
    assertEquals(L2D(o, Pt2D(0, -1), Color.GREEN), L2D(o, -90, 1, Color.GREEN))
  }


}
