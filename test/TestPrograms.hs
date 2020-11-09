module TestPrograms where

test_program_square_sum :: String
test_program_square_sum = "\
\   /* Calcular uma soma de quadrados. */       \
\   int main() {                                \
\       int s;                                  \
\       int n;                                  \
\       s = 0;                                  \
\       n = 1;                                  \
\       while (n <= 10) {                       \
\           s = s + n*n;                        \
\           n = n + 1;                          \
\       }                                       \
\       print_int(n);                           \
\   }"

test_program_integer_is_prime :: String
test_program_integer_is_prime = "\
\   /* Testar se um inteiro positivo é primo. */    \
\   bool is_prime(int n) {                          \
\       int d;                                      \
\       d = 2;                                      \
\       if (n == 1) // 1 não é primo\n              \
\           return false;                           \
\       while (d <= n) {                            \
\           if (n%d == 0)                           \
\               return false;                       \
\           else                                    \
\               d = d+1;                            \
\       }                                           \
\       return true;                                \
\   }                                               \
\                                                   \
\   int main() {                                    \
\       int n;                                      \
\       n = scan_int();                             \
\       if (is_prime(n))                            \
\           print_str(\"prime\");                   \
\       else                                        \
\           print_str(\"not prime\");               \
\   }"

test_program_factorial :: String
test_program_factorial = "\
\   /* Calcular factorial recursivamente */     \
\   int factorial(int n) {                      \
\       if (n == 0)                             \
\           return 1;                           \
\       return n * factorial(n-1);              \
\   }                                           \
\                                               \
\   int main() {                                \
\       print_int(factorial(read_int()));       \
\   }"