function addC(a, b, k) {
  return k(a + b);
}

function multC(a, b, k) {
  return k(a * b);
}

function subC(a, b, k) {
  return k(a - b);
}

subC(1,2, a => a)

function fact(n) {
  if (n === 1) {
    return 1;
  } else {
    return n * fact(n-1);
  }
}

function factC(n, k) {
  if (n === 1) {
    k(1);
  } else {
    subC(n, 1,
         subRes => factC(subRes,
             res => ))
  }
}
