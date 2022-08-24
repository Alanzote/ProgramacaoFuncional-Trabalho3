-- Aluno: Alan Renato Bunese
-- Disciplina: Programação Funcional
-- Professor: Frank Alcantara
module Main (main) where

{- 1
 - Escreva uma função para o cálculo dos números da sequência de Fibonacci,
 - utilizando Haskell.
 -}
fibonacci :: Int -> Int
fibonacci x
    | x < 0 = error "Não existe fibonacci de número negativo"
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fibonacci (x - 1) + fibonacci (x - 2)

{- 2
 - Um dos primeiros algoritimos documentados é o algoritimo para o cálculo
 - do Maior Divisor Comum (MDC) de Euclides publicado por volta do ano 300 AC.
 - Podemos simplificar este algoritimo dizendo que dados dos inteiros A e B, o
 - MDC entre eles será dado pelo valor absoluto de A se B = 0 e pelo MDC entre B
 - e o resto da divisão de A por B se B > 0. Escreva uma função para o cálculo
 - do MDC entre dois números inteiros posisitvos, usando o algoritimo de Euclides
 - conforme apresentado aqui, utilizando Haskell.
 -}
mdc :: Int -> Int -> Int
mdc x y
    | x < 0 || y < 0 = error "Não existe MDC de número negativo"
    | y == 0 = abs x
    | otherwise = mdc y (x `mod` y)

{- 3
 - Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos
 - deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e
 - recursividade.
 -}
somaDigitos :: Int -> Int
somaDigitos x
    | x < 0 = error "Não existe soma de dígitos de número negativo" -- Isso aqui teria como, mas não faço ideia qual seria a trativa correta.
    | x < 10 = x
    | otherwise = (x `mod` 10) + somaDigitos (x `div` 10)

{- 4
 - Escreva uma função que devolva a soma de todos os números menores que 10000 que
 - sejam múltiplos de 3 ou 5.
 -}
somaMultiplos3e5' :: Int -> Int
somaMultiplos3e5' x 
    | x < 0 = error "Não existe soma de múltiplos de número negativo"
    | x == 0 = 0
    | x `mod` 3 == 0 || x `mod` 5 == 0 = x + somaMultiplos3e5' (x - 1)
    | otherwise = somaMultiplos3e5' (x - 1)

somaMultiplos3e5 :: Int
somaMultiplos3e5 = somaMultiplos3e5' 10000

{- 5
 - Escreva uma função que, recebendo uma lista de inteiros, apresenta a diferença
 - entre a soma dos quadrados e o quadrado da some deste inteiros, usando recursividade.
 -}
somaQuadradosDiffQuadradoSoma' :: [Int] -> Int
somaQuadradosDiffQuadradoSoma' x
    | null x = 0
    | otherwise = (head x) ^ 2 + somaQuadradosDiffQuadradoSoma' (tail x)

somaQuadradosDiffQuadradoSoma'' :: [Int] -> Int
somaQuadradosDiffQuadradoSoma'' x
    | null x = 0
    | otherwise = (head x) + somaQuadradosDiffQuadradoSoma'' (tail x)

somaQuadradosDiffQuadradoSoma :: [Int] -> Int
somaQuadradosDiffQuadradoSoma x = (somaQuadradosDiffQuadradoSoma' x) - (somaQuadradosDiffQuadradoSoma'' x) ^ 2

{- 6
 - O Crivo de Eratóstenes não é o melhor algoritimo para encontrar números primos.
 - Cria uma função que implemente o Crivo de Euler (Euler's Sieve) para encontrar
 - todos os números primos menores que um determinado inteiro dado.
 -}
divs :: Int -> [Int]
divs n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = divs n == [1, n]

euler :: Int -> [Int]
euler n
    | n < 0 = error "Ignoramos negativos aqui"
    | otherwise = [ x | x <- [2..n], prime x ]

{- 7
 - Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva
 - todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123)
 - menores que um inteiro dado.
 -}
lucas' :: Int -> [Int] -> [Int]
lucas' x y
    | null y = []
    | head y < x = [head y] ++ lucas' x (tail y)
    | otherwise = lucas' x (tail y)

lucas :: Int -> [Int]
lucas x = lucas' x [2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123]

{- 8
 - Escreva uma função, chamada aoContrario em Haskell para reverter uma lista. Dado [1, 2, 3]
 - devolva [3, 2, 1].
 -}
aoContrario :: [Int] -> [Int]
aoContrario x
    | null x = []
    | otherwise = aoContrario (tail x) ++ [head x]

{- 9
 - Escreva uma função que, dado um inteiro n, devolva uma lista com todos os números
 - de 1 a n.
 -}
sequencia :: Int -> [Int]
sequencia x
    | x < 0 = error "Sequência não pode ser negativa"
    | x == 0 = []
    | x == 1 = [1]
    | otherwise = sequencia (x - 1) ++ [x]

{- 9
 - Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto
 - destes valores sem usar o operador de multiplicação.
 -}
somaRecursiva :: Int -> Int -> Int
somaRecursiva x y
    | y == 1 = x
    | otherwise = x + somaRecursiva x (y - 1)

{- 10
 - Escreva uma função chamda comprimento que receba uma lista de inteiros e devolva o comprimento
 - desta lista. Observe que você não pode usar nenhuma função que já calculo o comprimento de uma
 - lista.
 -}
comprimento :: [Int] -> Int
comprimento x
    | null x = 0
    | otherwise = 1 + comprimento (tail x)

-- Main...
main :: IO ()
main = do
    -- Um por linha...
    print ("fibonacci: entrada: 10; resultado: " ++ show (fibonacci 10))
    print ("fibonacci: entrada: 5; resultado: " ++ show (fibonacci 5))

    print ("mdc: entrada: 10 e 5; resultado: " ++ show (mdc 10 5))
    print ("mdc: entrada: 10 e 0; resultado: " ++ show (mdc 10 0))
    print ("mdc: entrada: 25 e 30; resultado: " ++ show (mdc 25 30))

    print ("somaDigitos: entrada: 1234; resultado: " ++ show (somaDigitos 1234))
    print ("somaDigitos: entrada: 5587; resultado: " ++ show (somaDigitos 5587))

    print ("somaMultiplos3e5: entrada: nenhuma; resultado: " ++ show somaMultiplos3e5)

    print ("somaQuadradosDiffQuadradoSoma: entrada: [1, 2, 3, 4, 5]; resultado: " ++ show (somaQuadradosDiffQuadradoSoma [1, 2, 3, 4, 5]))

    print ("euler: entrada: 10; resultado: " ++ show (euler 10))
    print ("euler: entrada: 20; resultado: " ++ show (euler 20))

    print ("lucas: entrada: 10; resultado: " ++ show (lucas 10))
    print ("lucas: entrada: 5; resultado: " ++ show (lucas 5))

    print ("aoContrario: entrada: [1, 2, 3]; resultado: " ++ show (aoContrario [1, 2, 3]))
    print ("aoContrario: entrada: [10, 15, 20, 25]; resultado: " ++ show (aoContrario [10, 15, 20, 25]))

    print ("sequencia: entrada: 10; resultado: " ++ show (sequencia 10))
    print ("sequencia: entrada: 25; resultado: " ++ show (sequencia 25))

    print ("somaRecursiva: entrada: 10, 5; resultado: " ++ show (somaRecursiva 10 5))
    print ("somaRecursiva: entrada: 20, 3; resultado: " ++ show (somaRecursiva 20 3))

    print ("comprimento: entrada: [1, 2, 3]; resultado: " ++ show (comprimento [1, 2, 3]))
    print ("comprimento: entrada: [10, 15, 20, 25]; resultado: " ++ show (comprimento [10, 15, 20, 25]))
