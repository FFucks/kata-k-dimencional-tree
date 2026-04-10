package com.ffucks.kdtree

import org.scalatest.funsuite.AnyFunSuite

class KDTreeTest extends AnyFunSuite {

    val points = Seq(
        Point(Vector(2, 3)),
        Point(Vector(5, 4)),
        Point(Vector(9, 6)),
        Point(Vector(4, 7)),
        Point(Vector(8, 1)),
        Point(Vector(7, 2))
    )

    val tree = new KDTree(points)

    test("should contain existing points") {
        assert(tree.contains(Point(Vector(2, 3))))
        assert(tree.contains(Point(Vector(5, 4))))
        assert(tree.contains(Point(Vector(9, 6))))
    }

    test("should not contain non-existing point") {
        assert(!tree.contains(Point(Vector(10, 10))))
        assert(!tree.contains(Point(Vector(0, 0))))
    }

    test("should handle empty tree") {
        val emptyTree = new KDTree(Seq.empty)
        assert(!emptyTree.contains(Point(Vector(1, 1))))
    }

    test("should work with single element") {
        val singleTree = new KDTree(Seq(Point(Vector(1, 1))))
        assert(singleTree.contains(Point(Vector(1, 1))))
        assert(!singleTree.contains(Point(Vector(2, 2))))
    }
}