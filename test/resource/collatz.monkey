let collatz = fn(x) {
  if (x == 1) {
    return x;
  }   
  let next = if (isEven(x)) {
    x / 2
  } else {
    x * 3 + 1
  };
  collatz(next)
};

let isEven = fn(x) {
  x % 2 == 0
};

collatz(7)