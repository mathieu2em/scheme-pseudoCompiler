{
  n = 1000;
  one = 1;
  while (n>0) { one = one*10; n = n-1; }
  a = one;
  x = one*one/2;
  r = x; while ((n = (r+x/r)/2) < r) r = n;
  t = one/4;
  p = 1;
  while (a != r) {
    x = a*r;
    y = (a+r)/2;
    z = y-a;
    a = y;
    r = x; while ((n = (r+x/r)/2) < r) r = n;
    t = t - p*z*z/one;
    p = p*2;
  }
  x = a+r;
  print(x*x/(4*t));
}
