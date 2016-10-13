rm(list = ls())
# ---------------------------------

# Cobb-Douglass
# author: Chris R. Eiler

# ---------------------------------

CobbDouglass <- function(alpha, beta, p, q, I, p2 = p, q2 = q, I2 = I) {
  # U(x, y) = alpha . ln(x) + beta . ln(y)
  # I = p . x + q . y
    alph = alpha / (alpha + beta)
    bet = beta / (alpha + beta)
    A <- matrix(c(p / alph, -q / bet, p, q), 2, 2, byrow = TRUE,
                dimnames = list(c(1, 2), c('x', 'y')))
          b <- matrix(c(0, I), 2)
          soln <- solve(A, b)
              lambda <- alph / (p * soln[ 1, ])
              cat('lambda: \n')
              print(lambda)
              cat('optimal values: \n')
              print(soln)
              cat('\n', 'Indirect Utility: \n')
            V <- alph * log(soln[1, ]) + bet * log(soln[2, ])
              print(V)
              cat('\n', '\n')
      if(p2 != p & q2 == q) {
      A.X <- matrix(c(p2 / alph, -q / bet, p2, q), 2, 2, byrow = TRUE,
                  dimnames = list(c(1, 2), c('x', 'y')))
         b <- matrix(c(0, I), 2)
        soln.X <- solve(A.X, b)
            lambda <- alph / (p2 * soln[ 1, ])
            cat('lambda: \n')
            print(lambda)
            cat('optimal values: \n')
            print(soln.X)
            cat('\n', 'Indirect Utility: \n')
           V.X <- alph * log(soln.X[1, ]) + bet * log(soln.X[2, ])
            print(V.X)
            cat('\n', '\n')
            cat('Substitution Effect: \n')
          Sub.Effect.X <- soln[1, ] - (((p2 / alph) ^ (alph - 1)) * ((q / bet) ^ bet) * V)
            print(Sub.Effect.X)
            cat('Income Effect: \n')
          Inc.Effect.X <- (soln.X[1, ] - soln[1,]) - Sub.Effect.X
            print(Inc.Effect.X)
        } else if(q2 != q & p2 == p) {
            A.Y <- matrix(c(p / alph, -q2 / bet, p, q2), 2, 2, byrow = TRUE,
                          dimnames = list(c(1, 2), c('x', 'y')))
              b <- matrix(c(0, I), 2)
              soln.Y <- solve(A.Y, b)
                lambda <- bet / (pq * soln[2, ])
                cat('lambda: \n')
                print(lambda)
                cat('optimal values: \n')
                print(soln.Y)
                cat('\n', 'Indirect Utility: \n')
                V.Y <- alph * log(soln.Y[1, ]) + bet * log(soln.Y[2, ])
                print(V.Y)
                cat('\n', '\n')
                cat('Substitution Effect: \n')
                Sub.Effect.Y <- soln[1, ] - (((q2 / bet) ^ (bet - 1)) * ((p / alph) ^ alph) * V)
                print(Sub.Effect.Y)
                cat('Income Effect: \n')
                Inc.Effect.Y <- (soln.X[1, ] - soln[1,]) - Sub.Effect.Y
                print(Inc.Effect.Y)
         } else if(p2 != p & q2 != q) {
            A2 <- matrix(c(p2 / alph, -q2 / bet, p2, q2), 2, 2, byrow = TRUE,
                       dimnames = list(c(1, 2), c('x2', 'y2')))
            soln2 <- solve(A2, b)
           lambda2 <- alph / (p2 * soln[1, ])
            cat('lambda: \n')
            print(lambda2)
            cat('optimal values: \n')
            print(soln2)
            cat('\n', 'Indirect Utility: \n')
          V4 <- alph * log(soln2[1, ]) + bet * log(soln2[2, ])
            print(V4)
            cat('\n', '\n')
      }
}

        
CobbDouglass(0.5, 0.5, 3, 3, 99)
CobbDouglass(5, 5, 3, 3, 99, p2 = 2)
CobbDouglass(0.2, 0.8, 2, 3, 200)
