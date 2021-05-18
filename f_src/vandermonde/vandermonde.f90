subroutine bivand1 ( n, alpha, beta, a )

!*****************************************************************************80
!
!! BIVAND1 returns a bidimensional Vandermonde1 matrix.
!
!  Discussion:
!
!    N = 3, ALPHA = ( 1, 2, 3 ), BETA = ( 10, 20, 30 )
!
!    (x,y)   | (1,10)  (2,10)  (3,10)  (1,20)  (2,20)  (1,30)
!    --------+-----------------------------------------------
!    1       |     1       1       1       1       1       1  
!    x       |     1       2       3       1       2       1
!       y    |    10      10      10      20      20      30
!    x^2     |     1       4       9       1       4       1
!    x  y    |    10      20      30      20      40      30
!    x^2y^2  |   100     100     100     400     400     900
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the data vectors.
!
!    Input, real ( kind = 8 ) ALPHA(N), BETA(N), the values that define A.
!
!    Output, real ( kind = 8 ) A(((N+1)*N)/2,((N+1)*N)/2), the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(((n+1)*n)/2,((n+1)*n)/2)
  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ) beta(n)
  integer ( kind = 4 ) e
  integer ( kind = 4 ) e1
  integer ( kind = 4 ) e2
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) jj
  integer ( kind = 4 ) n2

  n2 = ( n * ( n + 1 ) ) / 2

  e1 = 0
  e2 = 0
  e = 0

  do ii = 1, n2

    j1 = 1
    j2 = 1

    do jj = 1, n2

      if ( ii == 1 ) then
        a(ii,jj) = 1.0D+00
      else
        a(ii,jj) = alpha(j1) ** e1 * beta(j2) ** e2
      end if

      if ( j1 + j2 < n + 1 ) then
        j1 = j1 + 1
      else
        j1 = 1
        j2 = j2 + 1
      end if

    end do

    if ( e2 < e ) then
      e1 = e1 - 1
      e2 = e2 + 1
    else
      e = e + 1
      e1 = e
      e2 = 0
    end if

  end do

  return
end
subroutine bivand2 ( n, alpha, beta, a )

!*****************************************************************************80
!
!! BIVAND2 returns a bidimensional Vandermonde1 matrix.
!
!  Discussion:
!
!    N = 3, ALPHA = ( 1, 2, 3 ), BETA = ( 10, 20, 30 )
!
!    (x,y)   | (1,10) (2,10) (3,10) (1,20) (2,20) (3,20) (1,30) (2,30) (3,30)
!    --------+---------------------------------------------------------------
!    1       |     1      1      1      1      1      1      1      1      1  
!    x       |     1      2      3      1      2      1      1      2      3
!    x^2     |     1      4      9      1      4      1      1      4      9
!       y    |    10     10     10     20     20     20     30     30     30
!    x  y    |    10     20     30     20     40     60     30     60     90
!    x^2y    |    10     40     90     20     80    180     30    120    270
!       y^2  |   100    100    100    400    400    400    900    900    900
!    x  y^2  |   100    200    300    400    800   1200    900   1800   2700
!    x^2y^2  |   100    400    900    400   1600   3600    900   3600   8100
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the data vectors.
!
!    Input, real ( kind = 8 ) ALPHA(N), BETA(N), the values that define A.
!
!    Output, real ( kind = 8 ) A(N^2,N^2), the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n*n,n*n)
  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ) beta(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) jy

  i = 0
  do iy = 1, n
    do ix = 1, n
      i = i + 1
      j = 0
      do jy = 1, n
        do jx = 1, n
          j = j + 1
          a(i,j) = alpha(jx) ** ( ix - 1 ) * beta(jy) ** ( iy - 1 )
        end do
      end do
    end do
  end do

  return
end
subroutine dvand ( n, alpha, b, x )

!*****************************************************************************80
!
!! DVAND solves a Vandermonde system A' * x = b.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ake Bjorck, Victor Pereyra,
!    Solution of Vandermonde Systems of Equations,
!    Mathematics of Computation,
!    Volume 24, Number 112, October 1970, pages 893-903.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) ALPHA(N), the parameters that define the matrix.
!    The values should be distinct.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)

  x(1:n) = b(1:n)

  do k = 1, n - 1
    do j = n, k + 1, -1
      x(j) = ( x(j) - x(j-1) ) / ( alpha(j) - alpha(j-k) )
    end do
  end do

  do k = n - 1, 1, -1
    do j = k, n - 1
      x(j) = x(j) - alpha(k) * x(j+1)
    end do
  end do

  return
end
subroutine dvandprg ( n, alpha, b, x, c, m )

!*****************************************************************************80
!
!! DVANDPRG solves a Vandermonde system A' * x = f progressively.
!
!  Discussion:
!
!    This function receives the solution to the system of equations A' * x = f
!    where A is a Vandermonde matrix for alpha(0) through alpha(n-1),
!    and new values alpha(n) and f(n).  It updates the solution.
!
!    To solve a system of Nbig equations, this function may be called 
!    repeatedly, with N = 1, 2, ..., Nbig.  Each time, a solution to the 
!    current subsystem is returned.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ake Bjorck, Victor Pereyra,
!    Solution of Vandermonde Systems of Equations,
!    Mathematics of Computation,
!    Volume 24, Number 112, October 1970, pages 893-903.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the new order of the matrix, which is 1 
!    larger than on the previous call.  For the first call, N must be 1.
!
!    Input, real ( kind = 8 ) ALPHA(N), the parameters that define the matrix.
!    The values should be distinct.  The value ALPHA(N) has just been
!    added to the system.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Input/output, real ( kind = 8 ) X(N).  On input, the first N-1 entries 
!    contain the solution of the N-1xN-1 linear system.  On output, the 
!    solution to the NxN linear system.
!
!    Input/output, real ( kind = 8 ) C(N), M(N).  On input, the first N-1 
!    entries contain factorization data for the N-1xN-1 linear system.  On 
!    output, factorization data for the NxN linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) cn
  integer ( kind = 4 ) j
  real ( kind = 8 ) m(n)
  real ( kind = 8 ) x(n)

  c(n) = b(n)
  do j = n - 1, 1, -1
    c(j) = ( c(j+1) - c(j) ) / ( alpha(n) - alpha(j) )
  end do

  if ( n == 1 ) then
    m(n) = 1.0D+00
  else
    m(n) = 0.0D+00
  end if

  cn = c(1)
  x(n) = c(1)

  do j = n - 1, 1, -1
    m(j+1) = m(j+1) - alpha(n-1) * m(j)
    x(n-j) = x(n-j) + m(j+1) * cn
  end do

  return
end
subroutine pvand ( n, alpha, b, x )

!*****************************************************************************80
!
!! PVAND solves a Vandermonde system A * x = b.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ake Bjorck, Victor Pereyra,
!    Solution of Vandermonde Systems of Equations,
!    Mathematics of Computation,
!    Volume 24, Number 112, October 1970, pages 893-903.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) ALPHA(N), the parameters that define the matrix.
!    The values should be distinct.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)

  x(1:n) = b(1:n)

  do k = 1, n - 1
    do j = n, k + 1, -1
      x(j) = x(j) - alpha(k) * x(j-1)
    end do
  end do

  do k = n - 1, 1, -1
    do j = k + 1, n
      x(j) = x(j) / ( alpha(j) - alpha(j-k) )
    end do
    do j = k, n - 1
      x(j) = x(j) - x(j+1)
    end do
  end do

  return
end
subroutine pvandprg ( n, alpha, b, x, d, u )

!*****************************************************************************80
!
!! PVANDPRG solves a Vandermonde system A * x = f progressively.
!
!  Discussion:
!
!    This function receives the solution to the system of equations A * x = f
!    where A is a Vandermonde matrix for alpha(0) through alpha(n-1),
!    and new values alpha(n) and f(n).  It updates the solution.
!
!    To solve a system of Nbig equations, this function may be called 
!    repeatedly, with N = 1, 2, ..., Nbig.  Each time, a solution to the 
!    current subsystem is returned.
!
!    Note that the reference, which lists an Algol version of this algorithm, 
!    omits a minus sign, writing
!      u[j] := u[j] x delta;
!    where
!      u[j] := - u[j] x delta;
!    is actually necessary.  
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ake Bjorck, Victor Pereyra,
!    Solution of Vandermonde Systems of Equations,
!    Mathematics of Computation,
!    Volume 24, Number 112, October 1970, pages 893-903.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the new order of the matrix, which is 1 
!    larger than on the previous call.  For the first call, N must be 1.
!
!    Input, real ( kind = 8 ) ALPHA(N), the parameters that define the matrix.
!    The values should be distinct.  The value ALPHA(N) has just been
!    added to the system.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Input/output, real ( kind = 8 ) X(N); on input, the solution of the 
!    N-1xN-1 linear system.  On output, the solution of the NxN linear system.
!
!    Input/output, real ( kind = 8 ) D(N), U(N); on input, factorization data 
!    for the N-1xN-1 linear system.  On output, factorization data for the
!    NxN linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) delta
  real ( kind = 8 ) dn
  integer ( kind = 4 ) j
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) x(n)

  d(n) = b(n)
  do j = n - 1, 1, -1
    d(j) = d(j+1) - alpha(n-j) * d(j)
  end do

  dn = d(1)
  u(n) = 1.0D+00

  do j = 1, n - 1
    delta = alpha(n) - alpha(j)
    u(j) = - u(j) * delta
    u(n) = u(n) * delta
    x(j) = x(j) + dn / u(j)
  end do

  x(n) = dn / u(n)

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
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

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

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

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

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
!    An R8VEC is a vector of R8's.
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
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

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
subroutine vand1 ( n, x, a )

!*****************************************************************************80
!
!! VAND1 returns the Vandermonde1 matrix A with 1's on the first row.
!
!  Formula:
!
!    A(I,J) = X(J)^(I-1)
!
!  Example:
!
!    N = 5, X = ( 2, 3, 4, 5, 6 )
!
!    1  1   1   1   1
!    2  3   4   5   6
!    4  9  16  25  36
!    8 27  64 125  216
!   16 81 256 625 1296
!
!  Properties:
!
!    A is generally not symmetric: A' /= A.
!
!    A is nonsingular if, and only if, the X values are distinct.
!
!    det ( A ) = product ( 1 <= I <= N ) ( 1 <= J < I ) ( X(I) - X(J) ).
!             = product ( 1 <= J <= N ) X(J)
!             * product ( 1 <= I < J ) ( X(J) - X(I) ).
!
!    A is generally ill-conditioned.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Gregory, David Karney,
!    A Collection of Matrices for Testing Computational Algorithms,
!    Wiley, 1969, page 27,
!    LC: QA263.G68.
!
!    Nicholas Higham,
!    Stability analysis of algorithms for solving confluent
!    Vandermonde-like systems,
!    SIAM Journal on Matrix Analysis and Applications,
!    Volume 11, 1990, pages 23-41.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix desired.
!
!    Input, real ( kind = 8 ) X(N), the values that define A.
!
!    Output, real ( kind = 8 ) A(N,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)

  do i = 1, n

    do j = 1, n

      if ( i == 1 .and. x(j) == 0.0D+00 ) then
        a(i,j) = 1.0D+00
      else
        a(i,j) = x(j) ** ( i - 1 )
      end if

    end do
  end do

  return
end
