package org.example

import java.io.File
import kotlin.math.abs

fun main() {
    val input = File("input").readLines().map { it.split("   ").map { c -> c.toInt() } }
    val list1 = input.map { it -> it[0] }.sorted()
    val list2 = input.map { it -> it[1] }.sorted()

    // Part 1
    println(list1.zip(list2).map{ it -> abs (it.first - it.second) }.sum())

    // Part 2
    val set1 = list1.toSet()
    println(list2.map { if(set1.contains(it)) {it} else {0} }.sum())

}
