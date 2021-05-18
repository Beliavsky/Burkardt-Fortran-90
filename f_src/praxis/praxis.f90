function flin ( n, jsearch, l, f, x, nf, v, q0, q1, qd0, qd1, qa, qb, qc )

!*****************************************************************************80
!
!! FLIN is the function of one variable to be minimized by MINNY.
!
!  Discussion:
!
!    F(X) is a scalar function of a vector argument X.
!
!    A minimizer of F(X) is sought along a line or parabola.
!
!    This function has been modified, by removing the occurrence of a 
!    common block, so that it looks more like a "normal" function that does
!    not rely so strongly on peculiarities of FORTRAN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2016
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization with Derivatives,
!    Prentice Hall, 1973,
!    Reprinted by Dover, 2002.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, integer ( kind = 4 ) JSEARCH, indicates the kind of search.
!    If J is a legal column index, linear search in direction of V(*,JSEARCH).
!    Otherwise, then the search is parabolic, based on X, Q0 and Q1.
!
!    Input, real ( kind = 8 ) L, is the parameter determining the particular
!    point at which F is to be evaluated.  
!    For a linear search, L is the step size.
!    For a quadratic search, L is a parameter which specifies
!    a point in the plane of X, Q0 and Q1.
!
!    Input, external F, is the name of the function to be minimized.
!    The function should have the form 
!      function f(x,n)
!      integer ( kind = 4 ) n
!      real ( kind = 8 ) f
!      real ( kind = 8 ) x(n)
!    and accepts X and N as input, returning in F the function value.
!
!    Input, real ( kind = 8 ) X(N), the base point of the search.
!
!    Input/output, integer ( kind = 4 ) NF, the function evaluation counter.
!
!    Input, real ( kind = 8 ) V(N,N), a matrix whose columns constitute 
!    search directions.
!
!    Input, real ( kind = 8 ) Q0(N), Q1(N), two auxiliary points used to
!    determine the plane when a quadratic search is performed.
!
!    Input, real ( kind = 8 ) QD0, QD1, values needed to compute the 
!    coefficients QA, QB, QC.
!
!    Output, real ( kind = 8 ) QA, QB, QC, coefficients used to combine
!    Q0, X, and A1 if a quadratic search is used.
!
!    Output, real ( kind = 8 ) FLIN, the value of the function at the 
!    minimizing point.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), external :: f
  real ( kind = 8 ) flin
  integer ( kind = 4 ) jsearch
  real ( kind = 8 ) l
  integer ( kind = 4 ) nf
  real ( kind = 8 ) q0(n)
  real ( kind = 8 ) q1(n)
  real ( kind = 8 ) qa
  real ( kind = 8 ) qb
  real ( kind = 8 ) qc
  real ( kind = 8 ) qd0
  real ( kind = 8 ) qd1
  real ( kind = 8 ) t(n)
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) x(n)
!
!  The search is linear.
!
  if ( 1 <= jsearch ) then

    t(1:n) = x(1:n) + l * v(1:n,jsearch)
!
!  The search is along a parabolic space curve.
!
  else

    qa =                 l * ( l - qd1 ) /       ( qd0 + qd1 ) / qd0
    qb = - ( l + qd0 ) *     ( l - qd1 ) / qd1                 / qd0
    qc =   ( l + qd0 ) * l               / qd1 / ( qd0 + qd1 )

    t(1:n) = qa * q0(1:n) + qb * x(1:n) + qc * q1(1:n)

  end if
!
!  The function evaluation counter NF is incremented.
!
  nf = nf + 1
!
!  Evaluate the function.
!
  flin = f ( t, n )

  return
end
subroutine minfit ( n, tol, a, q )

!*****************************************************************************80
!
!! MINFIT computes the singular value decomposition of an N by N array.
!
!  Discussion:
!
!    This is an improved version of the EISPACK routine MINFIT
!    restricted to the case M = N and P = 0.
!
!    The singular values of the array A are returned in Q.  A is
!    overwritten with the orthogonal matrix V such that U * diag(Q) = A * V,
!    where U is another orthogonal matrix.
!
!    Thanks to Andreas Zuend for pointing out a potential for overflow in
!    the computation z = sqrt ( f*f + 1 ), 22 March 2012.
!
!    Thanks to Martin Horvat for correcting the initial assignment of S,
!    and for moving the line "L=I" back within the loop, 25 June 2013.
!
!    Thanks to Martin Horvat for moving the assignment "X=MAX(X,Y)"
!    into the loop, 29 October 2013.
!
!    Further modifications to this function were made, to remove the use
!    of statement labels, so that the function looks more like a "normal"
!    function that relies much less on the peculiarities of FORTRAN,
!    27 July 2016.
!
!    Removed argument M.  A must be dimensions (N,N).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2016
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization with Derivatives,
!    Prentice Hall, 1973,
!    Reprinted by Dover, 2002.
!
!    James Wilkinson, Christian Reinsch,
!    Handbook for Automatic Computation,
!    Volume II, Linear Algebra, Part 2,
!    Springer Verlag, 1971.
!
!    Brian Smith, James Boyle, Jack Dongarra, Burton Garbow, Yasuhiko Ikebe, 
!    Virginia Klema, Cleve Moler,
!    Matrix Eigensystem Routines, EISPACK Guide,
!    Lecture Notes in Computer Science, Volume 6,
!    Springer Verlag, 1976,
!    ISBN13: 978-3540075462,
!    LC: QA193.M37.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, real ( kind = 8 ) TOL, a tolerance which determines when a vector
!    (a column or part of a column of the matrix) may be considered
!    "essentially" equal to zero.
!
!    Input/output, real ( kind = 8 ) A(N,N).  On input, an N by N array whose
!    singular value decomposition is desired.  On output, the
!    SVD orthogonal matrix factor V.
!
!    Input/output, real ( kind = 8 ) Q(N), the singular values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c
  real ( kind = 8 ) e(n)
  real ( kind = 8 ) eps
  real ( kind = 8 ) f
  real ( kind = 8 ) g
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kt
  integer ( kind = 4 ), parameter :: kt_max = 30
  integer ( kind = 4 ) l
  integer ( kind = 4 ) l2
  real ( kind = 8 ) q(n)
  real ( kind = 8 ) r8_hypot
  real ( kind = 8 ) s
  logical skip
  real ( kind = 8 ) temp
  real ( kind = 8 ) tol
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z
!
!  Householder's reduction to bidiagonal form.
!
  if ( n == 1 ) then
    q(1) = a(1,1)
    a(1,1) = 1.0D+00
    return
  end if

  eps = epsilon ( eps )
  g = 0.0D+00
  x = 0.0D+00

  do i = 1, n

    e(i) = g
    l = i + 1

    s = sum ( a(i:n,i) ** 2 )

    g = 0.0D+00

    if ( tol <= s ) then

      f = a(i,i)

      g = sqrt ( s )
      if ( 0.0D+00 <= f ) then
        g = - g
      end if

      h = f * g - s
      a(i,i) = f - g

      do j = l, n

        f = dot_product ( a(i:n,i), a(i:n,j) ) / h

        a(i:n,j) = a(i:n,j) + f * a(i:n,i)

      end do 

    end if

    q(i) = g

    s = sum ( a(i,l:n) ** 2 )

    g = 0.0D+00

    if ( tol <= s ) then

      if ( i /= n ) then
        f = a(i,i+1)
      end if

      g = sqrt ( s )
      if ( 0.0D+00 <= f ) then
        g = - g
      end if

      h = f * g - s

      if ( i /= n ) then

        a(i,i+1) = f - g
        e(l:n) = a(i,l:n) / h

        do j = l, n

          s = dot_product ( a(j,l:n), a(i,l:n) )

          a(j,l:n) = a(j,l:n) + s * e(l:n)

        end do

      end if

    end if

    y = abs ( q(i) ) + abs ( e(i) )

    x = max ( x, y )

  end do
!
!  Accumulation of right-hand transformations.
!
  a(n,n) = 1.0D+00
  g = e(n)
  l = n

  do i = n - 1, 1, -1

    if ( g /= 0.0D+00 ) then

      h = a(i,i+1) * g

      a(l:n,i) = a(i,l:n) / h

      do j = l, n

        s = dot_product ( a(i,l:n), a(l:n,j) )

        a(l:n,j) = a(l:n,j) + s * a(l:n,i)

      end do

    end if

    a(i,l:n) = 0.0D+00
    a(l:n,i) = 0.0D+00
    a(i,i) = 1.0D+00

    g = e(i)

    l = i

  end do
!
!  Diagonalization of the bidiagonal form.
!
  eps = eps * x

  do k = n, 1, -1

    kt = 0

    do

      kt = kt + 1

      if ( kt_max < kt ) then
        e(k) = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MINFIT - Fatal error!'
        write ( *, '(a)' ) '  The QR algorithm failed to converge.'
        stop 1
      end if

      skip = .false.

      do l2 = k, 1, -1

        l = l2

        if ( abs ( e(l) ) <= eps ) then
          skip = .true.
          exit
        end if

        if ( l /= 1 ) then
          if ( abs ( q(l-1) ) <= eps ) then
            exit
          end if
        end if

      end do
!
!  Cancellation of E(L) if 1 < L.
!
      if ( .not. skip ) then

        c = 0.0D+00
        s = 1.0D+00

        do i = l, k
   
          f = s * e(i)
          e(i) = c * e(i)
          if ( abs ( f ) <= eps ) then
            exit
          end if
          g = q(i)
!
!  q(i) = h = sqrt(g*g + f*f).
!
          h = r8_hypot ( f, g )
  
          q(i) = h

          if ( h == 0.0D+00 ) then
            g = 1.0D+00
            h = 1.0D+00
          end if

          c =   g / h
          s = - f / h

        end do

      end if
!
!  Test for convergence for this index K.
!
      z = q(k)

      if ( l == k ) then
        if ( z < 0.0D+00 ) then
          q(k) = - z
          a(1:n,k) = - a(1:n,k)
        end if
        exit
      end if
!
!  Shift from bottom 2*2 minor.
!
      x = q(l)
      y = q(k-1)
      g = e(k-1)
      h = e(k)
      f = ( ( y - z ) * ( y + z ) + ( g - h ) * ( g + h ) ) &
        / ( 2.0D+00 * h * y )

      g = r8_hypot ( f, 1.0D+00 )

      if ( f < 0.0D+00 ) then
        temp = f - g
      else
        temp = f + g
      end if

      f = ( ( x - z ) * ( x + z ) + h * ( y / temp - h ) ) / x
!
!  Next QR transformation.
!
      c = 1.0D+00
      s = 1.0D+00

      do i = l + 1, k

        g = e(i)
        y = q(i)
        h = s * g
        g = g * c

        z = r8_hypot ( f, h )

        e(i-1) = z

        if ( z == 0.0D+00 ) then
          f = 1.0D+00
          z = 1.0D+00
        end if

        c = f / z
        s = h / z
        f =   x * c + g * s
        g = - x * s + g * c
        h = y * s
        y = y * c

        do j = 1, n
          x = a(j,i-1)
          z = a(j,i)
          a(j,i-1) = x * c + z * s
          a(j,i) = - x * s + z * c
        end do

        z = r8_hypot ( f, h )

        q(i-1) = z

        if ( z == 0.0D+00 ) then
          f = 1.0D+00
          z = 1.0D+00
        end if

        c = f / z
        s = h / z
        f =   c * g + s * y
        x = - s * g + c * y

      end do

      e(l) = 0.0D+00
      e(k) = f
      q(k) = x

    end do

  end do

  return
end
subroutine minny ( n, jsearch, nits, d2, x1, f1, fk, f, x, t, h, v, q0, q1, &
  nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 )

!*****************************************************************************80
!
!! MINNY minimizes a scalar function of N variables along a line.
!
!  Discussion:
!
!    MINNY minimizes F along the line from X in the direction V(*,J) unless
!    J is less than 1, when a quadratic search is made in the plane
!    defined by Q0, Q1 and X.
!
!    If FK = true, then F1 is FLIN(X1).  Otherwise X1 and F1 are ignored
!    on entry unless final FX is greater than F1.
!
!    This function was modified by removing the common blocks
!    and the use of labeled statements, 28 July 2016.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2016
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization with Derivatives,
!    Prentice Hall, 1973,
!    Reprinted by Dover, 2002.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, integer ( kind = 4 ) JSEARCH, indicates the kind of search.
!    If JSEARCH is a legal column index, linear search in direction of V(*,J).
!    Otherwise, the search is parabolic, based on X, Q0 and Q1.
!
!    Input, integer ( kind = 4 ) NITS, the maximum number of times the interval 
!    may be halved to retry the calculation.
!
!    Input/output, real ( kind = 8 ) D2, is either zero, or an approximation to 
!    the value of (1/2) times the second derivative of F.
!
!    Input/output, real ( kind = 8 ) X1, on entry, an estimate of the 
!    distance from X to the minimum along V(*,J), or, if J = 0, a curve.  
!    On output, the distance between X and the minimizer that was found.
!
!    Input/output, real ( kind = 8 ) F1, ?
!
!    Input, logical FK; if FK is TRUE, then on input F1 contains 
!    the value FLIN(X1).
!
!    Input, external real ( kind = 8 ) F, is the name of the function to 
!    be minimized.  The function should have the form 
!      function f(x,n)
!      integer ( kind = 4 ) n
!      real ( kind = 8 ) f
!      real ( kind = 8 ) x(n)
!    and accepts X and N as input, returning in F the function value.
!
!    Input/output, real ( kind = 8 ) X(N), ?
!
!    Input, real ( kind = 8 ) T, ?
!
!    Input, real ( kind = 8 ) H, ?
!
!    Input, real ( kind = 8 ) V(N,N), a matrix whose columns are direction
!    vectors along which the function may be minimized.
!
!    Input, real ( kind = 8 ) Q0(N), an auxiliary point used to define
!    a curve through X.
!
!    Input, real ( kind = 8 ) Q1(N), an auxiliary point used to define
!    a curve through X.
!
!    Input/output, integer ( kind = 4 ) NL, the number of linear searches.
!
!    Input/output, integer ( kind = 4 ) NF, the number of function evaluations.
!
!    Input, real ( kind = 8 ) DMIN, an estimate for the smallest eigenvalue.
!
!    Input, real ( kind = 8 ) LDT, the length of the step.
!
!    Input/output, real ( kind = 8 ) FX, the value of F(X,N).
!
!    Input/output, real ( kind = 8 ) QA, QB, QC, ?
!
!    Input, real ( kind = 8 ) QD0, QD1, ?.
!
  implicit none

  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  real ( kind = 8 ) dmin
  logical dz
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) f0
  real ( kind = 8 ) f1
  real ( kind = 8 ) f2
  logical fk
  real ( kind = 8 ) flin
  real ( kind = 8 ) fm
  real ( kind = 8 ) fx
  real ( kind = 8 ) h
  integer ( kind = 4 ) jsearch
  integer ( kind = 4 ) k
  real ( kind = 8 ) ldt
  real ( kind = 8 ) m2
  real ( kind = 8 ) m4
  real ( kind = 8 ) machep
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nf
  integer ( kind = 4 ) nits
  integer ( kind = 4 ) nl
  logical ok
  real ( kind = 8 ) q0(n)
  real ( kind = 8 ) q1(n)
  real ( kind = 8 ) qa
  real ( kind = 8 ) qb
  real ( kind = 8 ) qc
  real ( kind = 8 ) qd0
  real ( kind = 8 ) qd1
  real ( kind = 8 ) s
  real ( kind = 8 ) sf1
  real ( kind = 8 ) small
  real ( kind = 8 ) sx1
  real ( kind = 8 ) t
  real ( kind = 8 ) t2
  real ( kind = 8 ) temp
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) xm

  machep = epsilon ( machep )
  small = machep ** 2
  m2 = sqrt ( machep )
  m4 = sqrt ( m2 )
  sf1 = f1
  sx1 = x1
  k = 0
  xm = 0.0D+00
  fm = fx
  f0 = fx
  dz = ( d2 < machep )
!
!  Find the step size.
! 
  s = sqrt ( sum ( x(1:n) ** 2 ) )

  if ( dz ) then
    temp = dmin
  else
    temp = d2
  end if

  t2 = m4 * sqrt ( abs ( fx ) / temp + s * ldt ) + m2 * ldt
  s = m4 * s + t
  if ( dz .and. s < t2 ) then
    t2 = s
  end if

  t2 = max ( t2, small )
  t2 = min ( t2, 0.01D+00 * h )

  if ( fk .and. f1 <= fm ) then
    xm = x1
    fm = f1
  end if

  if ( .not. fk .or. abs ( x1 ) < t2 ) then

    if ( 0.0D+00 <= x1 ) then
      temp = 1.0D+00
    else
      temp = - 1.0D+00
    end if

    x1 = temp * t2
    f1 = flin ( n, jsearch, x1, f, x, nf, v, q0, q1, qd0, qd1, qa, qb, qc )

  end if

  if ( f1 <= fm ) then
    xm = x1
    fm = f1
  end if
!
!  Evaluate FLIN at another point and estimate the second derivative.
!
  do

    if ( dz ) then

      if ( f1 <= f0 ) then
        x2 = 2.0D+00 * x1
      else
        x2 = - x1
      end if

      f2 = flin ( n, jsearch, x2, f, x, nf, v, q0, q1, qd0, qd1, qa, qb, qc )

      if ( f2 <= fm ) then
        xm = x2
        fm = f2
      end if

      d2 = ( x2 * ( f1 - f0 ) - x1 * ( f2 - f0 ) ) &
        / ( ( x1 * x2 ) * ( x1 - x2 ) )

    end if
!
!  Estimate the first derivative at 0.
!
    d1 = ( f1 - f0 ) / x1 - x1 * d2
    dz = .true.
!
!  Predict the minimum.
!
    if ( d2 <= small ) then

      if ( 0.0D+00 <= d1 ) then
        x2 = - h
      else
        x2 = h
      end if

    else

      x2 = ( - 0.5D+00 * d1 ) / d2

    end if

    if ( h < abs ( x2 ) ) then

      if ( x2 <= 0.0D+00 ) then
        x2 = - h
      else
        x2 = h
      end if

    end if
!
!  Evaluate F at the predicted minimum.
!
    ok = .true.

    do

      f2 = flin ( n, jsearch, x2, f, x, nf, v, q0, q1, qd0, qd1, qa, qb, qc )

      if ( nits <= k .or. f2 <= f0 ) then
        exit
      end if

      k = k + 1

      if ( f0 < f1 .and. 0.0D+00 < x1 * x2 ) then
        ok = .false.
        exit
      end if

      x2 = 0.5D+00 * x2

    end do

    if ( ok ) then
      exit
    end if

  end do
!
!  Increment the one-dimensional search counter.
!
  nl = nl + 1

  if ( fm < f2 ) then
    x2 = xm
  else
    fm = f2
  end if
!
!  Get a new estimate of the second derivative.
!
  if ( small < abs ( x2 * ( x2 - x1 ) ) ) then
    d2 = ( x2 * ( f1 - f0 ) - x1 * ( fm - f0 ) ) / ( ( x1 * x2 ) * ( x1 - x2 ) )
  else
    if ( 0 < k ) then
      d2 = 0.0D+00
    end if
  end if

  d2 = max ( d2, small )

  x1 = x2
  fx = fm

  if ( sf1 < fx ) then
    fx = sf1
    x1 = sx1
  end if
!
!  Update X for linear but not parabolic search.
!
  if ( 1 <= jsearch ) then

    x(1:n) = x(1:n) + x1 * v(1:n,jsearch)

  end if

  return
end
function praxis ( t0, h0, n, prin, x, f )

!*****************************************************************************80
!
!! PRAXIS seeks an N-dimensional minimizer X of a scalar function F(X).
!
!  Discussion:
!
!    PRAXIS returns the minimum of the function F(X,N) of N variables
!    using the principal axis method.  The gradient of the function is
!    not required.
!
!    The approximating quadratic form is
!
!      Q(x') = F(x,n) + (1/2) * (x'-x)' * A * (x'-x)
!
!    where X is the best estimate of the minimum and 
!
!      A = inverse(V') * D * inverse(V)
!
!    V(*,*) is the matrix of search directions; 
!    D(*) is the array of second differences.  
!
!    If F(X) has continuous second derivatives near X0, then A will tend 
!    to the hessian of F at X0 as X approaches X0.
!
!    Thanks to Andreas Zuend for pointing out an error in the form of the
!    call to the routine r8mat_print (), 22 March 2012.
!
!    This function was modified by eliminating the use of labeled statements,
!    and removing the common blocks, 28 July 2016.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization with Derivatives,
!    Prentice Hall, 1973,
!    Reprinted by Dover, 2002.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T0, is a tolerance.  PRAXIS attempts to return 
!    praxis = f(x) such that if X0 is the true local minimum near X, then
!    norm ( x - x0 ) < T0 + sqrt ( EPSILON ( X ) ) * norm ( X ),
!    where EPSILON ( X ) is the machine precision for X.
!
!    Input, real ( kind = 8 ) H0, is the maximum step size.  H0 should be 
!    set to about the maximum distance from the initial guess to the minimum.
!    If H0 is set too large or too small, the initial rate of
!    convergence may be slow.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, integer ( kind = 4 ) PRIN, controls printing intermediate results.
!    0, nothing is printed.
!    1, F is printed after every n+1 or n+2 linear minimizations.  
!       final X is printed, but intermediate X is printed only 
!       if N is at most 4.
!    2, the scale factors and the principal values of the approximating 
!       quadratic form are also printed.
!    3, X is also printed after every few linear minimizations.
!    4, the principal vectors of the approximating quadratic form are 
!       also printed.
!
!    Input/output, real ( kind = 8 ) X(N), is an array containing on entry a
!    guess of the point of minimum, on return the estimated point of minimum.
!
!    Input, external real ( kind = 8 ) F, is the name of the function to be
!    minimized.  The function should have the form 
!      function f(x,n)
!      integer ( kind = 4 ) n
!      real ( kind = 8 ) f
!      real ( kind = 8 ) x(n)
!    and accepts X and N as input, returning in F the function value.
!
!    Output, real ( kind = 8 ) PRAXIS, the function value at the minimizer.
!
!  Local parameters:
!
!    Local, real ( kind = 8 ) DMIN, an estimate for the smallest eigenvalue.
!
!    Local, real ( kind = 8 ) FX, the value of F(X,N).
!
!    Local, logical ILLC, is TRUE if the system is ill-conditioned.
!
!    Local, real ( kind = 8 ) LDT, the length of the step.
!
!    Local, integer ( kind = 4 ) NF, the number of function evaluations.
!
!    Local, integer ( kind = 4 ) NL, the number of linear searches.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) d(n)
  real ( kind = 8 ) d2
  real ( kind = 8 ) df
  real ( kind = 8 ) dmin
  real ( kind = 8 ) dn
  real ( kind = 8 ) dni
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) f1
  logical fk
  real ( kind = 8 ) fx
  real ( kind = 8 ) h
  real ( kind = 8 ) h0
  integer ( kind = 4 ) i
  logical illc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jsearch
  integer ( kind = 4 ) k
  integer ( kind = 4 ) k2
  integer ( kind = 4 ) kl
  integer ( kind = 4 ) kt
  integer ( kind = 4 ) ktm
  real ( kind = 8 ) large
  real ( kind = 8 ) ldfac
  real ( kind = 8 ) lds
  real ( kind = 8 ) ldt
  real ( kind = 8 ) m2
  real ( kind = 8 ) m4
  real ( kind = 8 ) machep
  integer ( kind = 4 ) nits
  integer ( kind = 4 ) nl
  integer ( kind = 4 ) nf
  real ( kind = 8 ) praxis
  integer ( kind = 4 ) prin
  real ( kind = 8 ) q0(n)
  real ( kind = 8 ) q1(n)
  real ( kind = 8 ) qa
  real ( kind = 8 ) qb
  real ( kind = 8 ) qc
  real ( kind = 8 ) qd0
  real ( kind = 8 ) qd1
  real ( kind = 8 ) qf1
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) s
  real ( kind = 8 ) scbd
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sf
  real ( kind = 8 ) sl
  real ( kind = 8 ) small
  real ( kind = 8 ) t
  real ( kind = 8 ) t0
  real ( kind = 8 ) t2
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) value
  real ( kind = 8 ) vlarge
  real ( kind = 8 ) vsmall
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)
!
!  Initialization.
!
  machep = epsilon ( machep )
  small = machep * machep
  vsmall = small * small
  large = 1.0D+00 / small
  vlarge = 1.0D+00 / vsmall
  m2 = sqrt ( machep )
  m4 = sqrt ( m2 )
  seed = 123456789
!
!  Heuristic numbers:
!
!  If the axes may be badly scaled (which is to be avoided if
!  possible), then set SCBD = 10.  Otherwise set SCBD = 1.
!
!  If the problem is known to be ill-conditioned, initialize ILLC = true.
!
!  KTM is the number of iterations without improvement before the
!  algorithm terminates.  KTM = 4 is very cautious; usually KTM = 1
!  is satisfactory.
!
  scbd = 1.0D+00
  illc = .false.
  ktm = 1

  if ( illc ) then
    ldfac = 0.1D+00
  else
    ldfac = 0.01D+00
  end if

  kt = 0
  nl = 0
  nf = 1
  fx = f ( x, n )
  qf1 = fx
  t = small + abs ( t0 )
  t2 = t
  dmin = small
  h = h0
  h = max ( h, 100.0D+00 * t )
  ldt = h
!
!  The initial set of search directions V is the identity matrix.
!
  v(1:n,1:n) = 0.0D+00
  do i = 1, n
    v(i,i) = 1.0D+00
  end do

  d(1:n) = 0.0D+00
  qa = 0.0D+00
  qb = 0.0D+00
  qc = 0.0D+00
  qd0 = 0.0D+00
  qd1 = 0.0D+00
  q0(1:n) = x(1:n)
  q1(1:n) = x(1:n)

  if ( 0 < prin ) then
    call print2 ( n, x, prin, fx, nf, nl )
  end if
!
!  The main loop starts here.
!
  do

    sf = d(1)
    d(1) = 0.0D+00
!
!  Minimize along the first direction V(*,1).
!
    jsearch = 1
    nits = 2
    d2 = d(1)
    s = 0.0D+00
    value = fx
    fk = .false.

    call minny ( n, jsearch, nits, d2, s, value, fk, f, x, t, &
      h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 )

    d(1) = d2

    if ( s <= 0.0D+00 ) then
      v(1:n,1) = - v(1:n,1)
    end if

    if ( sf <= 0.9D+00 * d(1) .or. d(1) <= 0.9D+00 * sf ) then
      d(2:n) = 0.0D+00
    end if
!
!  The inner loop starts here.
!
    do k = 2, n

      y(1:n) = x(1:n)

      sf = fx

      if ( 0 < kt ) then
        illc = .true.
      end if

      do

        kl = k
        df = 0.0D+00
!
!  A random step follows, to avoid resolution valleys.
!
        if ( illc ) then

          do i = 1, n
            r = r8_uniform_01 ( seed )
!           call random_number ( harvest = r )
            s = ( 0.1D+00 * ldt + t2 * 10.0D+00 ** kt ) * ( r - 0.5D+00 )
            z(i) = s
            x(1:n) = x(1:n) + s * v(1:n,i)
          end do

          fx = f ( x, n )
          nf = nf + 1

        end if
!
!  Minimize along the "non-conjugate" directions V(*,K),...,V(*,N).
!
        do k2 = k, n

          sl = fx

          jsearch = k2
          nits = 2
          d2 = d(k2)
          s = 0.0D+00
          value = fx
          fk = .false.

          call minny ( n, jsearch, nits, d2, s, value, fk, f, x, t, &
            h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 )

          d(k2) = d2

          if ( illc ) then
            s = d(k2) * ( ( s + z(k2) ) ** 2 )
          else
            s = sl - fx
          end if

          if ( df <= s ) then
            df = s
            kl = k2
          end if

        end do
!
!  If there was not much improvement on the first try, set
!  ILLC = true and start the inner loop again.
!
        if ( illc ) then
          exit
        end if

        if ( abs ( 100.0D+00 * machep * fx ) <= df ) then
          exit
        end if

        illc = .true.

      end do

      if ( k == 2 .and. 1 < prin ) then
        call r8vec_print ( n, d, '  The second difference array:' )
      end if
!
!  Minimize along the "conjugate" directions V(*,1),...,V(*,K-1).
!
      do k2 = 1, k - 1

        jsearch = k2
        nits = 2
        d2 = d(k2)
        s = 0.0D+00
        value = fx
        fk = .false.

        call minny ( n, jsearch, nits, d2, s, value, fk, f, x, t, &
          h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 )

        d(k2) = d2

      end do

      f1 = fx
      fx = sf
      lds = 0.0D+00

      do i = 1, n
        sl = x(i)
        x(i) = y(i)
        sl = sl - y(i)
        y(i) = sl
        lds = lds + sl ** 2
      end do

      lds = sqrt ( lds )
!
!  Discard direction V(*,kl).
!
!  If no random step was taken, V(*,KL) is the "non-conjugate"
!  direction along which the greatest improvement was made.
!
      if ( small < lds ) then

        do j = kl - 1, k, -1
          v(1:n,j+1) = v(1:n,j)
          d(j+1) = d(j)
        end do

        d(k) = 0.0D+00

        v(1:n,k) = y(1:n) / lds
!
!  Minimize along the new "conjugate" direction V(*,k), which is
!  the normalized vector:  (new x) - (old x).
!
        jsearch = k
        nits = 4
        d2 = d(k)
        value = f1
        fk = .true.

        call minny ( n, jsearch, nits, d2, lds, value, fk, f, x, t, &
          h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 )

        d(k) = d2

        if ( lds <= 0.0D+00 ) then
          lds = - lds
          v(1:n,k) = - v(1:n,k)
        end if

      end if

      ldt = ldfac * ldt
      ldt = max ( ldt, lds )

      if ( 0 < prin ) then
        call print2 ( n, x, prin, fx, nf, nl )
      end if

      t2 = m2 * sqrt ( sum ( x(1:n) ** 2 ) ) + t
!
!  See whether the length of the step taken since starting the
!  inner loop exceeds half the tolerance.
!
      if ( 0.5D+00 * t2 < ldt ) then
        kt = - 1
      end if

      kt = kt + 1

      if ( ktm < kt ) then

        if ( 0 < prin ) then
          call r8vec_print ( n, x, '  X:' )
        end if

        praxis = fx

        return

      end if

    end do
!
!  The inner loop ends here.
!
!  Try quadratic extrapolation in case we are in a curved valley.
!
    call quad ( n, f, x, t, h, v, q0, q1, nl, nf, dmin, ldt, fx, qf1, &
      qa, qb, qc, qd0, qd1 )

    d(1:n) = 1.0D+00 / sqrt ( d(1:n) )

    dn = maxval ( d(1:n) )

    if ( 3 < prin ) then
      call r8mat_print ( n, n, v, '  The new direction vectors:' )
    end if

    do j = 1, n
      v(1:n,j) = ( d(j) / dn ) * v(1:n,j)
    end do
!
!  Scale the axes to try to reduce the condition number.
!
    if ( 1.0D+00 < scbd ) then

      do i = 1, n
        z(i) = max ( m4, sqrt ( sum ( v(i,1:n) ** 2 ) ) )
      end do

      s = minval ( z(1:n) )

      do i = 1, n

        sl = s / z(i)
        z(i) = 1.0D+00 / sl

        if ( scbd < z(i) ) then
          sl = 1.0D+00 / scbd
          z(i) = scbd
        end if

        v(i,1:n) = sl * v(i,1:n)

      end do

    end if
!
!  Calculate a new set of orthogonal directions before repeating
!  the main loop.
!
!  Transpose V for MINFIT:
!
    v(1:n,1:n) = transpose ( v(1:n,1:n) )
!
!  Call MINFIT to find the singular value decomposition of V.
!
!  This gives the principal values and principal directions of the
!  approximating quadratic form without squaring the condition number.
!
    call minfit ( n, vsmall, v, d )
!
!  Unscale the axes.
!
    if ( 1.0D+00 < scbd ) then

      do i = 1, n
        v(i,1:n) = z(i) * v(i,1:n)
      end do

      do i = 1, n

        s = sqrt ( sum ( v(1:n,i) ** 2 ) )
        d(i) = s * d(i)
        v(1:n,i) = v(1:n,i) / s

      end do

    end if

    do i = 1, n

      dni = dn * d(i)

      if ( large < dni ) then
        d(i) = vsmall
      else if ( dni < small ) then
        d(i) = vlarge
      else
        d(i) = 1.0D+00 / dni ** 2
      end if

    end do
!
!  Sort the singular values and singular vectors.
!
    call svsort ( n, d, v )
!
!  Determine the smallest eigenvalue.
!
    dmin = max ( d(n), small )
!
!  The ratio of the smallest to largest eigenvalue determines whether
!  the system is ill conditioned.
!
    if ( dmin < m2 * d(1) ) then
      illc = .true.
    else
      illc = .false.
    end if

    if ( 1 < prin ) then

      if ( 1.0D+00 < scbd ) then
        call r8vec_print ( n, z, '  The scale factors:' )
      end if 

      call r8vec_print ( n, d, '  Principal values of the quadratic form:' )

    end if

    if ( 3 < prin ) then
      call r8mat_print ( n, n, v, '  The principal axes:' )
    end if
!
!  The main loop ends here.
!
  end do

  if ( 0 < prin ) then
    call r8vec_print ( n, x, '  X:' )
  end if

  praxis = fx

  return
end
subroutine print2 ( n, x, prin, fx, nf, nl )

!*****************************************************************************80
!
!! PRINT2 prints certain data about the progress of the iteration.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 May 2006
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization with Derivatives,
!    Prentice Hall, 1973,
!    Reprinted by Dover, 2002.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the current estimate of the minimizer.
!
!    Input, integer ( kind = 4 ) PRIN, the user-specifed print level.
!    0, nothing is printed.
!    1, F is printed after every n+1 or n+2 linear minimizations.  
!       final X is printed, but intermediate X is printed only 
!       if N is at most 4.
!    2, the scale factors and the principal values of the approximating 
!       quadratic form are also printed.
!    3, X is also printed after every few linear minimizations.
!    4, the principal vectors of the approximating quadratic form are 
!       also printed.
!
!    Input, real ( kind = 8 ) FX, the smallest value of F(X) found so far.
!
!    Input, integer ( kind = 4 ) NF, the number of function evaluations.
!
!    Input, integer ( kind = 4 ) NL, the number of linear searches.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx
  integer ( kind = 4 ) nf
  integer ( kind = 4 ) nl
  integer ( kind = 4 ) prin
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Linear searches      ', nl
  write ( *, '(a,i8)' ) '  Function evaluations ', nf 

  if ( n <= 4 .or. 2 < prin ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'X:'
    write ( *, '(5g14.6)' ) x(1:n)
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The function value FX: ', fx

  return
end
subroutine quad ( n, f, x, t, h, v, q0, q1, nl, nf, dmin, ldt, fx, qf1, &
  qa, qb, qc, qd0, qd1 )

!*****************************************************************************80
!
!! QUAD seeks to minimize the scalar function F along a particular curve.
!
!  Discussion:
!
!    The minimizer to be sought is required to lie on a curve defined
!    by Q0, Q1 and X.
!
!    This function was modified by removing the common blocks,
!    28 July 2016.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2016
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization with Derivatives,
!    Prentice Hall, 1973,
!    Reprinted by Dover, 2002.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, external real ( kind = 8 ) F, is the name of the function to 
!    be minimized.  The function should have the form 
!      function f(x,n)
!      integer ( kind = 4 ) n
!      real ( kind = 8 ) f
!      real ( kind = 8 ) x(n)
!    and accepts X and N as input, returning in F the function value.
!
!    Input/output, real ( kind = 8 ) X(N), ?
!
!    Input, real ( kind = 8 ) T, ?
!
!    Input, real ( kind = 8 ) H, ?
!
!    Input, real ( kind = 8 ) V(N,N), the matrix of search directions.
!
!    Input/output, real ( kind = 8 ) Q0(N), Q1(N), auxiliary points used to \
!    define a curve through X.
!
!    Input/output, integer ( kind = 4 ) NL, the number of linear searches.
!
!    Input/output, integer ( kind = 4 ) NF, the number of function evaluations.
!
!    Input, real ( kind = 8 ) DMIN, an estimate for the smallest eigenvalue.
!
!    Input, real ( kind = 8 ) LDT, the length of the step.
!
!    Input/output, real ( kind = 8 ) FX, the value of F(X,N).
!
!    Input/output, real ( kind = 8 ) QF1, QA, QB, QC, QD0, QD1 ?
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dmin
  real ( kind = 8 ), external :: f
  logical fk
  real ( kind = 8 ) fx
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jsearch
  real ( kind = 8 ) l
  real ( kind = 8 ) ldt
  integer ( kind = 4 ) nf
  integer ( kind = 4 ) nits
  integer ( kind = 4 ) nl
  real ( kind = 8 ) q0(n)
  real ( kind = 8 ) q1(n)
  real ( kind = 8 ) qa
  real ( kind = 8 ) qb
  real ( kind = 8 ) qc
  real ( kind = 8 ) qd0
  real ( kind = 8 ) qd1
  real ( kind = 8 ) qf1
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) temp
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)

  temp = fx
  fx   = qf1
  qf1  = temp

  call r8vec_swap ( n, x, q1 )

  qd1 = sqrt ( sum ( ( x(1:n) - q1(1:n) ) ** 2 ) )

  l = qd1
  s = 0.0D+00

  if ( qd0 <= 0.0D+00 .or. qd1 <= 0.0D+00 .or. nl < 3 * n * n ) then

    fx = qf1
    qa = 0.0D+00
    qb = 0.0D+00
    qc = 1.0D+00

  else

    jsearch = -1
    nits = 2
    value = qf1
    fk = .true.

    call minny ( n, jsearch, nits, s, l, value, fk, f, x, t, &
      h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 )

    qa =                 l * ( l - qd1 )       / ( qd0 + qd1 ) / qd0
    qb = - ( l + qd0 )     * ( l - qd1 ) / qd1                 / qd0
    qc =   ( l + qd0 ) * l               / qd1 / ( qd0 + qd1 )

  end if

  qd0 = qd1

  do i = 1, n
    s = q0(i)
    q0(i) = x(i)
    x(i) = qa * s + qb * x(i) + qc * q1(i)
  end do

  return
end
function r8_hypot ( x, y )

!*****************************************************************************80
!
!! R8_HYPOT returns the value of sqrt ( X^2 + Y^2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, Y, the arguments.
!
!    Output, real ( kind = 8 ) R8_HYPOT, the value of sqrt ( X^2 + Y^2 ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8_hypot
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( abs ( x ) < abs ( y ) ) then
    a = abs ( y )
    b = abs ( x )
  else
    a = abs ( x )
    b = abs ( y )
  end if
!
!  A contains the larger value.
!
  if ( a == 0.0D+00 ) then
    c = 0.0D+00
  else
    c = a * sqrt ( 1.0D+00 + ( b / a ) ** 2 )
  end if

  r8_hypot = c

  return
end
function r8_uniform_01 ( seed )

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,2x,g16.8)' ) i, a(i)
  end do

  return
end
subroutine r8vec_swap ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_SWAP swaps the entries of two R8VECs.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the arrays.
!
!    Input/output, real ( kind = 8 ) A1(N), A2(N), the vectors to swap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) a3(n)

  a3(1:n) = a1(1:n)
  a1(1:n) = a2(1:n)
  a2(1:n) = a3(1:n)

  return
end
subroutine svsort ( n, d, v ) 

!*****************************************************************************80
!
!! SVSORT descending sorts singular values D and adjusts V.
!
!  Discussion:
!
!    A simple bubble sort is used on D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 February 2002
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization with Derivatives,
!    Prentice Hall, 1973,
!    Reprinted by Dover, 2002.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of D, and the order of V.
!
!    Input/output, real ( kind = 8 ) D(N), the vector to be sorted.  
!    On output, the entries of D are in descending order.
!
!    Input/output, real ( kind = 8 ) V(N,N), an N by N array to be adjusted 
!    as D is sorted.  In particular, if the value that was in D(I) on input is
!    moved to D(J) on output, then the input column V(*,I) is moved to
!    the output column V(*,J).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) d(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j3
  real ( kind = 8 ) t
  real ( kind = 8 ) v(n,n)

  do j = 1, n - 1

    j3 = j;
    do j2 = j + 1, n
      if ( d(j3) < d(j2) ) then
        j3 = j2
      end if
    end do

    t     = d(j)
    d(j)  = d(j3)
    d(j3) = t

    do i = 1, n
      t       = v(i,j)
      v(i,j)  = v(i,j3)
      v(i,j3) = t
    end do

  end do

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
