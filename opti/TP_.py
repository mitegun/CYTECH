import numpy as np

#Question 1: implémentation de g(x) sous forme quadratique
def g_moindresC(x):
    B = np.array([[2, 9, 1], [0, 1, 0], [1, 0, 1], [1, 0, -2]])
    beta = np.array([[1], [1], [2], [1]])
    A = np.dot(np.transpose(B), B)
    b = np.dot(B.T, beta)
    y = np.dot(b.T, x) + 0.5 * np.dot(x.T, np.dot(A, x))
    return y

#Question 2: implémentation du gradient de g(x) sous forme quadratique
def grad_g_moindresC(x):
    B = np.array([[2, 9, 1], [0, 1, 0], [1, 0, 1], [1, 0, -2]])
    beta = np.array([[1], [1], [2], [1]])
    A = np.dot(np.transpose(B), B)
    b = np.dot(B.T, beta)
    gradient = np.dot(A, x) + b
    return gradient

#Question 3   
x0 = np.array([[2], [-1], [0]])
print("La valeur de g(x0):", g_moindresC(x0))
print("\n La valeur du gradient de g(x0):", grad_g_moindresC(x0))

#Question 4: Implémentation de la méthode du gradient à pas de Cauchy
def gradient_pas_cauchy():
    k = 0
    eps = 1e-7
    ITER_MAX = 500
    B = np.array([[2, 9, 1], [0, 1, 0], [1, 0, 1], [1, 0, -2]])
    beta = np.array([[1], [1], [2], [1]])
    A = np.dot(np.transpose(B), B)
    b = np.dot(B.T, beta)
    x0 = np.array([[2], [-1], [0]])
    x = x0
    gradient = grad_g_moindresC(x)
    while k < ITER_MAX:
        d = -gradient
        pas = (np.linalg.norm(gradient) ** 2) / np.dot(d.T, np.dot(A, d))
        x1 = x + pas * d
        if np.linalg.norm(x1 - x) < eps * np.linalg.norm(x):
            break
        x = x1
        gradient = grad_g_moindresC(x)
        k += 1
    print("Gradient à pas de Cauchy\n Le résultat en", k, "itérations est:", x, "\nle pas était:", pas, "le résiduel",np.linalg.norm(gradient),"\n")

#Question 5: Implémentation de la méthode du gradient à pas d'Armijo
def gradient_armijo():
    rho = 1
    tau = 0.7
    omg = 1e-4
    k = 0
    eps = 1e-7
    ITER_MAX = 500
    x0 = np.array([[2], [-1], [0]]) 
    d = -grad_g_moindresC(x0)    
    while k < ITER_MAX:
        d = -grad_g_moindresC(x0)
        f_x0 = g_moindresC(x0)
        x_prev = x0  
        while g_moindresC(x0 + rho * d) > f_x0 + omg * rho * np.dot(d.T, d):
            rho = tau * rho
            k += 1
        x0 = x0 + rho * d
        k += 1
        if np.linalg.norm(x0 - x_prev) < eps * np.linalg.norm(x_prev):
            break
    print("Gradient à pas d'Armijo\n Le résultat en", k, "itérations est:", x0, "\nle pas était:", rho, "le résiduel", np.linalg.norm(-d),"\n")

#Question 6: Implémentation de la méthode du gradient conjugué à pas de Cauchy
def gradient_conjugue():
    k = 0
    eps = 1e-7
    ITER_MAX = 500
    B = np.array([[2, 9, 1], [0, 1, 0], [1, 0, 1], [1, 0, -2]])
    beta = np.array([[1], [1], [2], [1]])
    A = np.dot(np.transpose(B), B)
    b = np.dot(B.T, beta)
    x0 = np.array([[2], [-1], [0]])
    x = x0
    grad = grad_g_moindresC(x)
    d = -grad
    k = 0
    while k < ITER_MAX:
        pas = np.dot(grad.T, grad) / np.dot(d.T, np.dot(A, d))
        x = x + pas * d
        grad_1 = grad + pas * np.dot(A, d)
        beta = np.dot(grad_1.T, grad_1) / np.dot(grad.T, grad)
        d = -grad_1 + beta * d
        grad = grad_1
        k += 1
        if np.linalg.norm(x - x0) < eps * np.linalg.norm(x0):
            break
        x0 = x
    print("Gradient conjugué à pas de Cauchy\n Le résultat en", k, "itérations est:", x, "\n le pas était:", pas, "le résiduel", np.linalg.norm(grad),"\n")

#Appel des fonctions
gradient_pas_cauchy()
gradient_armijo()
gradient_conjugue()
