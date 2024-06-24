import numpy as np

def gradient_pas_fixe():
    k = 0
    iterMax=200
    eps=10^-4
    A = np.array([[10, 0], [0, 1]])  
    b = np.array([-3, -3])
    x0 = np.array([-2, -7])
    x = x0
    alpha = 0.1 
    def f(x):
        return 0.5 * np.dot(x, np.dot(A, x)) - np.dot(b, x)

    while k < iterMax:
        gradient = np.dot(A, x) + b
        gradient_norm = np.linalg.norm(gradient)
        if gradient_norm < eps:
            break
        x = x - alpha * gradient
        k += 1
    print("Le résultat en", k, "itérations est:", x)

def gradient_pas_opti():
    k = 0
    eps=1e-4
    iterMax=200
    A = np.array([[10, 0], [0, 1]])  
    b = np.array([-3, -3])
    x0 = np.array([-2, -7])
    x = x0
    while k < iterMax:
        gradient = np.dot(A, x) + b
        gradient_norm = np.linalg.norm(gradient)
        if gradient_norm < eps:
            break
        d = -gradient
        alpha = (gradient_norm ** 2) / np.dot(d, np.dot(A, d))
        x = x + alpha * d
        k += 1
        
    print("Le résultat en", k, "itérations est:", x)


def gradient_armijo():
    k = 0
    rho=1
    tau=0.7
    omg=1e-4
    iterMax=200
    eps=1e-6
    A = np.array([[10, 0], [0, 1]])
    b = np.array([-3, -3])
    x0 = np.array([-2, -7])
    d0 = -(np.dot(A, x0) + b)    
    def f(x):
        y = np.dot(b, x) + 0.5 * np.dot(x, np.dot(A, x))
        return y

    while k < iterMax:
        d = -(np.dot(A, x0) + b)
        while f(x0 + rho * d) > f(x0) + omg * rho * np.dot(d, d):
            rho = tau * rho
            k = k + 1
        x0 = x0 + rho * d
        k = k + 1
        if np.linalg.norm(d) < eps:
            break

    print("Le résultat en", k, "itérations est:", x0)


def gradient_conjugue():
    A = np.array([[10, 0], [0, 1]]) 
    b = np.array([-3, -3])  
    x0 = np.array([-2, -7])  
    eps = 1e-6
    IterMax = 1000
    x = x0
    r = np.dot(A, x) + b
    p = -r
    k = 0
    
    while k < IterMax and np.linalg.norm(r) > eps:
        alpha = np.dot(r, r) / np.dot(p, np.dot(A, p))
        x = x + alpha * p
        r_next = r + alpha * np.dot(A, p)
        beta = np.dot(r_next, r_next) / np.dot(r, r)
        p = -r_next + beta * p
        r = r_next
        k += 1

    print("Le résultat en", k, "itérations est:", x)

gradient_armijo()
gradient_pas_fixe()
gradient_pas_opti()
gradient_conjugue()