package org.example.aoc.day10

import java.io.File

fun main() {
    val xs = sequence {
        val input = File("../input").readText().trim()
        var x = 1
        for (line in input.lines()) {
            val words = line.split(' ')
            if (words[0] == "addx") {
                yield(x)
                yield(x)
                x += words[1].toInt()
            } else {
                yield(x)
            }
        }
    }

    val firstAnswer = xs.withIndex()
        .drop(19)
        .filterIndexed { n, _ -> n % 40 == 0 }
        .take(6)
        .sumOf { (it.index + 1) * it.value }
    println("First answer: $firstAnswer")

    println("Second answer:")
    val screenCoordinates = generateSequence(0, Int::inc).map { it % 40 }
    screenCoordinates.zip(xs)
        .map { (screenX, x) ->
            if ((x - 1) <= screenX && screenX <= (x + 1)) '#' else '.'
        }
        .chunked(40)
        .map { it.joinToString("") }
        .forEach(::println)
}
