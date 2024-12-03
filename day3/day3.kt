package org.example

import java.io.File

fun main() {

    val input = File("input").readText()

    // Part 1
    val THEMAP = (0..1000).map {x ->
        (0..1000).map { y -> Pair("($x,$y)", x * y)}
    }.flatten().toMap()

    val THEFUNCTION = { inp : String -> THEMAP.map { (k, v) ->
        inp.split("mul").count { i -> i.startsWith(k) } * v }.sum() }
    println( THEFUNCTION(input) )

    // Part 2
    val THEINPUT = "do()" + input

    println(THEINPUT.split("don't()").map {
            str -> str.split("do()").drop(1).map {
                THEFUNCTION(it)
        }
    }.flatten().sum())


}
