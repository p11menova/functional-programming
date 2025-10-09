package com.example

import java.math.BigInteger


fun getCertainDigitsFib(targetDigits: Int): Long {
    val limit: BigInteger = BigInteger.TEN.pow(targetDigits - 1);
    var a = BigInteger.ONE;
    var b = BigInteger.ONE;
    var n: Long = 2;
    while (b <= limit) {
        n++;
        val tmp = a;
        a = b;
        b += tmp;
    }
    return n;
}