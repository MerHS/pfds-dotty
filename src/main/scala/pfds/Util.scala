package net.kinetc.pfds

import scala.math.Ordering

object Util {
  given Ordering[Nothing] {
    def compare(a: Nothing, b: Nothing) = 0
  }
}