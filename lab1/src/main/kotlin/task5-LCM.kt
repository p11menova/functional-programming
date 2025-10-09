package com.example;


fun gcd(x: Int, y: Int): Int {
    var a = x;
    var b = y;
    while (b != 0) {
        val remainder = a % b
        a = b
        b = remainder
    }
    return a

}

fun lcm(x: Int, y: Int): Int {
    return x * y / gcd(x, y);
}

fun lcmFromOneToTarget(a: Int): Int {
    var result = 1;
    for (i in 1..a) {
        result = lcm(result, i);
    }
    return result
}