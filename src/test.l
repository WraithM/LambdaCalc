main = add two one;

true = \a b. a;
false = \a b. b;
if = \p a b. p a b;

zero = \f x. x;
one = \f x. f x;
two = \f x. f (f x);

add = \m n. \f x. m f (n f x);

test = \x. x + 5;
