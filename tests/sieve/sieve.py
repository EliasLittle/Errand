#!/usr/bin/env python3
"""Sieve of Eratosthenes — same idea as tests/sieve/sieve.err (count primes in [2, n])."""


def sieve_prime_count(n: int) -> int:
    if n < 2:
        return 0
    is_prime = [True] * (n + 1)
    is_prime[0] = is_prime[1] = False
    p = 2
    while p <= n:
        if is_prime[p]:
            m = p * p
            while m <= n:
                is_prime[m] = False
                m += p
        p += 1
    return sum(1 for i in range(2, n + 1) if is_prime[i])


def main() -> None:
    n = 10000000
    print(sieve_prime_count(n))


if __name__ == "__main__":
    main()
