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

        def search(node: Option[KDNode],
                   best: Option[Point],
                   bestDist: Double): (Option[Point], Double) = {

            if (node.isEmpty) {
                return (best, bestDist)
            }

            val n = node.get

            val currentDist = target.distance(n.point)

            var currentBest = best
            var currentBestDist = bestDist

            if (currentDist < currentBestDist) {
                currentBest = Some(n.point)
                currentBestDist = currentDist
            }

            val axis = n.axis
            val goLeft = target.coordenates(axis) < n.point.coordenates(axis)

            val firstBranch = if (goLeft) n.left else n.right

            val secondBranch = if (goLeft) n.right else n.left

            val (bestAfterFirst, distAfterFirst) =
                search(firstBranch, currentBest, currentBestDist)

            currentBest = bestAfterFirst
            currentBestDist = distAfterFirst

            val axisDistance = math.abs(target.coordenates(axis) - n.point.coordenates(axis))

            if (axisDistance < currentBestDist) {
                val (bestAfterSecond, distAfterSecond) =
                    search(secondBranch, currentBest, currentBestDist)

                currentBest = bestAfterSecond
                currentBestDist = distAfterSecond
            }

            (currentBest, currentBestDist)
        }

        val (bestPoint, _) = search(root, None, Double.MaxValue)

        bestPoint
    }
}