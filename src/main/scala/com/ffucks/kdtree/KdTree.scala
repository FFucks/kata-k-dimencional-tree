package com.ffucks.kdtree

case class Point(coordenates: Vector[Double]) {

    def dimension: Int = {
        coordenates.length
    }

    def distance(other: Point): Double = {
        math.sqrt(
            coordenates
                .zip(other.coordenates)
                .map { case (a, b) =>
                    math.pow(a - b, 2)
                }
                .sum
        )
    }
}

case class KDNode(point: Point,
                 left: Option[KDNode],
                 right: Option[KDNode],
                 axis: Int)

class KDTree(points: Seq[Point]) {

    val k: Int = {
        if (points.isEmpty) 0 else points.head.dimension
    }

    val root: Option[KDNode] = {
        build(points.toVector, depth = 0)
    }

    private def build(points: Vector[Point], depth: Int): Option[KDNode] = {
        if (points.isEmpty) {
            return None
        }

        val axis = depth % k

        val sorted = {
            points.sortBy(p => p.coordenates(axis))
        }

        val medianIdx = {
            sorted.length / 2
        }

        Some(
            KDNode(
                point = sorted(medianIdx),
                left = build(sorted.take(medianIdx), depth + 1),
                right = build(sorted.drop(medianIdx + 1), depth + 1),
                axis = axis
            )
        )
    }

    def contains(target: Point): Boolean = {

        def search(node: Option[KDNode], depth: Int): Boolean = {
            node match {
                case None => false

                case Some(n) => {
                    if (n.point == target) {
                        return true
                    }

                    val axis = depth % k

                    if (target.coordenates(axis) < n.point.coordenates(axis)) {
                        search(n.left, depth + 1)
                    } else {
                        search(n.right, depth + 1)
                    }
                }
            }
        }

        search(root, 0)
    }

    def nearestNeighbor(target: Point): Option[Point] = {

        def nn(node: Option[KDNode],
              depth: Int,
              best: Option[Point])
        : Option[Point] = {

            node match {
                case None => best

                case Some(n) => {

                    val axis = depth % k

                    val next = {
                        if (target.coordenates(axis) < n.point.coordenates(axis)) n.left
                        else n.right
                    }

                    val other = {
                        if (next == n.left) n.right else n.left
                    }

                    val bestUpdated = {
                        best match {
                            case None => Some(n.point)

                            case Some(b) => {
                                if (target.distance(n.point) < target.distance(b)) {
                                    Some(n.point)
                                } else {
                                    best
                                }
                            }
                        }
                    }

                    val bestAfterNext = {
                        nn(next, depth + 1, bestUpdated)
                    }

                    val shouldCheckOther = {
                        bestAfterNext match {
                            case None => true

                            case Some(b) => {
                                math.abs(target.coordenates(axis) - n.point.coordenates(axis)) <
                                    target.distance(b)
                            }
                        }
                    }

                    if (shouldCheckOther) {
                        nn(other, depth + 1, bestAfterNext)
                    } else {
                        bestAfterNext
                    }
                }
            }
        }

        nn(root, 0, None)
    }
}