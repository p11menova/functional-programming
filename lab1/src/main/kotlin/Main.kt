package com.example


fun main() {
    print("введите число: ")
    val a: Int = readln().toInt()

    println("НОК(1..$a) = ${lcmFromOneToTarget(a)}")
    print("введите необходимое кол-во цифр числа Фибоначчи: ")
    println("индекс первого числа Фибоначчи с заданным числом цифр: " + getCertainDigitsFib(readln().toInt()))
}