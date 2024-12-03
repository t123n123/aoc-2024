package org.example

import java.io.File
import kotlin.math.abs

fun main() {

    val input = File("input").readLines().map{ x -> x.split(" ").map(String::toInt)}

    val check = { x:List<Int> ->
            (x == x.sorted() || x == x.sortedDescending())
            && x.zipWithNext{ a, b -> abs(a - b) in 1..3 }.all {it}}

    // Part 1
    println (input.count(check))

    // Part 2
    println (input.count{ ls -> ls.indices.map { check( ls.subList(0, it) + ls.subList(it + 1, ls.size))}.any {it} })

}
