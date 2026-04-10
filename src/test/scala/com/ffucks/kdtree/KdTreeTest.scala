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

    test("should contain existing point") {
        assert(tree.contains(Point(Vector(5, 4))))
    }

    test("should not contain non-existing point") {
        assert(!tree.contains(Point(Vector(10, 10))))
    }

    test("nearest neighbor should return closest point") {
        val target = Point(Vector(9, 2))
        val nearest = tree.nearestNeighbor(target)

        assert(nearest.contains(Point(Vector(8, 1))))
    }

    test("nearest neighbor exact match") {
        val target = Point(Vector(2, 3))
        val nearest = tree.nearestNeighbor(target)

        assert(nearest.contains(Point(Vector(2, 3))))
    }
}