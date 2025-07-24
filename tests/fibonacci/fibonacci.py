#!/usr/bin/env python
# -*- coding: utf-8 -*-

def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n-2)

print(fibonacci(35))
