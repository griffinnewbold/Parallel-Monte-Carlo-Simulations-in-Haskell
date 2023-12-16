def bernoulli(p):
    if random.random() < p:
        return True
    else:
        return False

def monte_carlo_asian_call(n, t, r, u, d, s_o, k):
    discount = 1 / ((1 + r) ** t)
    p_star = (1 + r - d) / (u - d)
    total = 0

    for i in range(n):
        sum_prices = 0
        price = s_o
        for i in range(t):
            if bernoulli(p_star):
                price=price*u
            else:
                price=price*d
            sum_prices += price
        val = (sum_prices / t) - k 
        total += max(val, 0)
      
    return (total * discount) / n
