subroutine bartlett_sample ( m, df, sigma, t )

!*****************************************************************************80
!
!! BARTLETT_SAMPLE samples the Bartlett distribution.
!
!  Discussion:
!
!    If the matrix T is sampled from the Bartlett distribution, then 
!    the matrix W = T' * T is a sample from the Wishart distribution.
!
!    This function requires functions from the PDFLIB and RNGLIB libraries.
!
!    The "initialize()" function from RNGLIB must be called before using
!    this function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Patrick Odell, Alan Feiveson,
!    A numerical procedure to generate a sample covariance matrix,
!    Journal of the American Statistical Association,
!    Volume 61, Number 313, March 1966, pages 199-203.
!
!    Stanley Sawyer,
!    Wishart Distributions and Inverse-Wishart Sampling,
!    Washington University,
!    30 April 2007, 12 pages.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the matrix.
!
!    Input, integer ( kind = 4 ) DF, the number of degrees of freedom.
!    M <= DF.
!
!    Input, real ( kind = 8 ) SIGMA(M,M), the covariance matrix, which should be 
!    a symmetric positive definite matrix.
!
!    Output, real ( kind = 8 ) T(M,M), the sample matrix from 
!    the Bartlett distribution.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) df
  integer ( kind = 4 ) flag;
  real ( kind = 8 ), allocatable :: r(:,:)
  real ( kind = 8 ) sigma(m,m)
  real ( kind = 8 ) t(m,m)
  real ( kind = 8 ), allocatable :: tu(:,:)

  if ( df < m ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BARTLETT_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
    stop 1
  end if
!
!  Get the upper triangular Cholesky factor of SIGMA.
!
  allocate ( r(1:m,1:m) )
  call r8mat_cholesky_factor_upper ( m, sigma, r, flag )

  if ( flag /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'BARTLETT_SAMPLE - Fatal error!'
    write ( *, '(a)' ) &
      '  Unexpected error return from R8MAT_CHOLESKY_FACTOR_UPPER.'
    write ( *, '(a,i4)' ) '  FLAG = ', flag
    stop 1
  end if
!
!  Sample the unit Bartlett distribution.
!
  allocate ( tu(1:m,1:m) )
  call bartlett_unit_sample ( m, df, tu )
!
!  Construct the matrix T = TU * R.
!
  t = matmul ( tu(1:m,1:m), r(1:m,1:m) )
!
!  Free memory.
!
  deallocate ( r )
  deallocate ( tu )

  return
end
subroutine bartlett_unit_sample ( m, df, t )

!*****************************************************************************80
!
!! BARTLETT_UNIT_SAMPLE samples the unit Bartlett distribution.
!
!  Discussion:
!
!    If the matrix T is sampled from the unit Bartlett distribution, then 
!    the matrix W = T' * T is a sample from the unit Wishart distribution.
! 
!    This function requires functions from the PDFLIB and RNGLIB libraries.
!
!    The "initialize()" function from RNGLIB must be called before using
!    this function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Patrick Odell, Alan Feiveson,
!    A numerical procedure to generate a sample covariance matrix,
!    Journal of the American Statistical Association,
!    Volume 61, Number 313, March 1966, pages 199-203.
!
!    Stanley Sawyer,
!    Wishart Distributions and Inverse-Wishart Sampling,
!    Washington University,
!    30 April 2007, 12 pages.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the matrix.
!
!    Input, integer ( kind = 4 ) DF, the number of degrees of freedom.
!    M <= DF.
!
!    Output, real ( kind = 8 ) T(M,M), the sample matrix from the 
!    unit Bartlett distribution.
!
  implicit none

  integer ( kind = 4 ) m

  integer ( kind = 4 ) df
  real ( kind = 8 ) df_chi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_chi_sample
  real ( kind = 8 ) r8_normal_01_sample
  real ( kind = 8 ) t(m,m)

  if ( df < m ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BARTLETT_UNIT_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
    stop 1
  end if

  do i = 1, m

    t(i,1:i-1) = 0.0D+00

    df_chi = real ( df + 1 - i, kind = 8 )
    t(i,i) = sqrt ( r8_chi_sample ( df_chi ) )

    do j = i + 1, m
      t(i,j) = r8_normal_01_sample ( )
    end do

  end do

  return
end
subroutine jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

!*****************************************************************************80
!
!! JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
!
!  Discussion:
!
!    This function computes the eigenvalues and eigenvectors of a
!    real symmetric matrix, using Rutishauser's modfications of the classical
!    Jacobi rotation method with threshold pivoting. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 July 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix, which must be square, real,
!    and symmetric.
!
!    Input, integer ( kind = 4 ) IT_MAX, the maximum number of iterations.
!
!    Output, real ( kind = 8 ) V(N,N), the matrix of eigenvectors.
!
!    Output, real ( kind = 8 ) D(N), the eigenvalues, in descending order.
!
!    Output, integer ( kind = 4 ) IT_NUM, the total number of iterations.
!
!    Output, integer ( kind = 4 ) ROT_NUM, the total number of rotations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) bw(n)
  real ( kind = 8 ) c
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) g
  real ( kind = 8 ) gapq
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) p
  integer ( kind = 4 ) q
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tau
  real ( kind = 8 ) term
  real ( kind = 8 ) termp
  real ( kind = 8 ) termq
  real ( kind = 8 ) theta
  real ( kind = 8 ) thresh
  real ( kind = 8 ) v(n,n)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) zw(n)

  call r8mat_identity ( n, v )

  call r8mat_diag_get_vector ( n, a, d )

  bw(1:n) = d(1:n)
  zw(1:n) = 0.0D+00
  it_num = 0
  rot_num = 0

  do while ( it_num < it_max )

    it_num = it_num + 1
!
!  The convergence threshold is based on the size of the elements in
!  the strict upper triangle of the matrix.
!
    thresh = 0.0D+00
    do j = 1, n
      do i = 1, j - 1
        thresh = thresh + a(i,j) ** 2
      end do
    end do

    thresh = sqrt ( thresh ) / real ( 4 * n, kind = 8 )

    if ( thresh == 0.0D+00 ) then
      exit 
    end if

    do p = 1, n
      do q = p + 1, n

        gapq = 10.0D+00 * abs ( a(p,q) )
        termp = gapq + abs ( d(p) )
        termq = gapq + abs ( d(q) )
!
!  Annihilate tiny offdiagonal elements.
!
        if ( 4 < it_num .and. &
             termp == abs ( d(p) ) .and. &
             termq == abs ( d(q) ) ) then

          a(p,q) = 0.0D+00
!
!  Otherwise, apply a rotation.
!
        else if ( thresh <= abs ( a(p,q) ) ) then

          h = d(q) - d(p)
          term = abs ( h ) + gapq

          if ( term == abs ( h ) ) then
            t = a(p,q) / h
          else
            theta = 0.5D+00 * h / a(p,q)
            t = 1.0D+00 / ( abs ( theta ) + sqrt ( 1.0D+00 + theta * theta ) )
            if ( theta < 0.0D+00 ) then 
              t = - t
            end if
          end if

          c = 1.0D+00 / sqrt ( 1.0D+00 + t * t )
          s = t * c
          tau = s / ( 1.0D+00 + c )
          h = t * a(p,q)
!
!  Accumulate corrections to diagonal elements.
!
          zw(p) = zw(p) - h                  
          zw(q) = zw(q) + h
          d(p) = d(p) - h
          d(q) = d(q) + h

          a(p,q) = 0.0D+00
!
!  Rotate, using information from the upper triangle of A only.
!
          do j = 1, p - 1
            g = a(j,p)
            h = a(j,q)
            a(j,p) = g - s * ( h + g * tau )
            a(j,q) = h + s * ( g - h * tau )
          end do

          do j = p + 1, q - 1
            g = a(p,j)
            h = a(j,q)
            a(p,j) = g - s * ( h + g * tau )
            a(j,q) = h + s * ( g - h * tau )
          end do

          do j = q + 1, n
            g = a(p,j)
            h = a(q,j)
            a(p,j) = g - s * ( h + g * tau )
            a(q,j) = h + s * ( g - h * tau )
          end do
!
!  Accumulate information in the eigenvector matrix.
!
          do j = 1, n
            g = v(j,p)
            h = v(j,q)
            v(j,p) = g - s * ( h + g * tau )
            v(j,q) = h + s * ( g - h * tau )
          end do

          rot_num = rot_num + 1

        end if

      end do
    end do

    bw(1:n) = bw(1:n) + zw(1:n)
    d(1:n) = bw(1:n)
    zw(1:n) = 0.0D+00

  end do
!
!  Restore upper triangle of input matrix.
!
  do j = 1, n
    do i = 1, j - 1
      a(i,j) = a(j,i)
    end do
  end do
!
!  Descending sort the eigenvalues and eigenvectors.
!
  do k = 1, n - 1

    m = k

    do l = k + 1, n
      if ( d(m) < d(l) ) then
        m = l
      end if
    end do

    if ( m /= k ) then

      t    = d(m)
      d(m) = d(k)
      d(k) = t

      w(1:n)   = v(1:n,m)
      v(1:n,m) = v(1:n,k)
      v(1:n,k) = w(1:n)

    end if

  end do

  return
end
function r8_epsilon ( )

!*****************************************************************************80
!
!! R8_EPSILON returns the R8 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_EPSILON, the round-off unit.
!
  implicit none

  real ( kind = 8 ) r8_epsilon

  r8_epsilon = 2.220446049250313D-016

  return
end
subroutine r8mat_cholesky_factor_upper ( n, a, c, flag )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_FACTOR_UPPER: upper Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is an upper triangular matrix R such that:
!
!      A = R * R'
!
!    The lower Cholesky factor is a lower triangular matrix L such that
!
!      A = L * L'
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Output, real ( kind = 8 ) C(N,N), the N by N upper triangular
!    Cholesky factor.
!
!    Output, integer ( kind = 4 ) FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!    2, the matrix is not nonnegative definite.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) flag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) sum2

  flag = 0;

  c(1:n,1:n) = a(1:n,1:n);

  do j = 1, n

    c(j,1:j-1) = 0.0D+00

    do i = j, n

      sum2 = c(i,j) - dot_product ( c(1:j-1,j), c(1:j-1,i) )

      if ( i == j ) then
        if ( sum2 <= 0.0D+00 ) then
          flag = 1
          return
        else
          c(j,i) = sqrt ( sum2 )
        end if
      else
        if ( c(j,j) /= 0.0D+00 ) then
          c(j,i) = sum2 / c(j,j)
        else
          c(j,i) = 0.0D+00
        end if
      end if

    end do

  end do

  return
end
subroutine r8mat_diag_get_vector ( n, a, v )

!*****************************************************************************80
!
!! R8MAT_DIAG_GET_VECTOR gets the value of the diagonal of an R8MAT.
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
!    22 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Output, real ( kind = 8 ) V(N), the diagonal entries
!    of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  do i = 1, n
    v(i) = a(i,i)
  end do

  return
end
subroutine r8mat_diagonal ( n, diag, a )

!*****************************************************************************80
!
!! R8MAT_DIAGONAL returns a diagonal matrix as an R8MAT.
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
!    31 July 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Input, real ( kind = 8 ) DIAG(N), the diagonal entries.
!
!    Output, real ( kind = 8 ) A(N,N), the N by N diagonal matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) diag(n)
  integer ( kind = 4 ) i

  a(1:n,1:n) = 0.0D+00

  do i = 1, n
    a(i,i) = diag(i)
  end do

  return
end
subroutine r8mat_identity ( n, a )

!*****************************************************************************80
!
!! R8MAT_IDENTITY stores the identity matrix in an R8MAT.
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
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Output, real ( kind = 8 ) A(N,N), the N by N identity matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i

  a(1:n,1:n) = 0.0D+00

  do i = 1, n
    a(i,i) = 1.0D+00
  end do

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
subroutine r8ut_inverse ( n, a )

!*****************************************************************************80
!
!! R8UT_INVERSE computes the inverse of an R8UT matrix.
!
!  Discussion:
!
!    The R8UT storage format is used for an M by N upper triangular 
!    matrix.  The format stores all M*N entries, even those which are zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input/output, real ( kind = 8 ) A(N,N).
!    On input, the upper triangular matrix to be inverted.
!    On output, the inverse of the upper triangular matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
!
!  Check.
!
  do i = 1, n
    if ( a(i,i) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8UT_INVERSE - Fatal error!'
      write ( *, '(a)' ) '  Zero diagonal element.'
      stop 1
    end if
  end do

  do j = n, 1, -1

    do i = n, 1, -1

      if ( j < i ) then

        a(i,j) = 0.0D+00

      else if ( i == j ) then

        a(i,j) = 1.0D+00 / a(i,j)

      else if ( i < j ) then

        a(i,j) = - sum ( a(i,i+1:j) * a(i+1:j,j) ) / a(i,i)

      end if

    end do
  end do

  return
end
subroutine wishart_sample ( m, df, sigma, a )

!*****************************************************************************80
!
!! WISHART_SAMPLE samples the Wishart distribution.
!
!  Discussion:
!
!    This function requires functions from the PDFLIB and RNGLIB libraries.
!
!    The "initialize()" function from RNGLIB must be called before using
!    this function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Patrick Odell, Alan Feiveson,
!    A numerical procedure to generate a sample covariance matrix,
!    Journal of the American Statistical Association,
!    Volume 61, Number 313, March 1966, pages 199-203.
!
!    Stanley Sawyer,
!    Wishart Distributions and Inverse-Wishart Sampling,
!    Washington University,
!    30 April 2007, 12 pages.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the matrix.
!
!    Input, integer ( kind = 4 ) DF, the number of degrees of freedom.
!    M <= DF.
!
!    Input, real ( kind = 8 ) SIGMA(M,M), the covariance matrix, which should be 
!    a symmetric positive definite matrix.
!
!    Output, real ( kind = 8 ) A(M,M), the sample matrix from 
!    the Wishart distribution.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a(m,m)
  real ( kind = 8 ), allocatable :: au(:,:)
  real ( kind = 8 ), allocatable :: aur(:,:)
  integer ( kind = 4 ) df
  integer ( kind = 4 ) flag
  real ( kind = 8 ), allocatable :: r(:,:)
  real ( kind = 8 ) sigma(m,m)

  if ( df < m ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WISHART_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
    stop 1
  end if
!
!  Get R, the upper triangular Cholesky factor of SIGMA.
!
  allocate ( r(1:m,1:m) )
  call r8mat_cholesky_factor_upper ( m, sigma, r, flag )

  if ( flag /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'WISHART_SAMPLE - Fatal error!'
    write ( *, '(a)' ) &
      '  Unexpected error return from R8MAT_CHOLESKY_FACTOR_UPPER.'
    write ( *, '(a,i4)' ) '  FLAG = ', flag
    stop 1
  end if
!
!  Get AU, a sample from the unit Wishart distribution.
!
  allocate ( au(1:m,1:m) )
  call wishart_unit_sample ( m, df, au )
!
!  Construct the matrix A = R' * AU * R.
!
  allocate ( aur(1:m,1:m) )
  aur = matmul ( au(1:m,1:m), r(1:m,1:m) )

  a = matmul ( transpose ( r(1:m,1:m) ), aur(1:m,1:m) )
!
!  Free memory.
!
  deallocate ( au )
  deallocate ( aur )
  deallocate ( r )

  return
end
subroutine wishart_sample_inverse ( m, df, sigma, a )

!*****************************************************************************80
!
!! WISHART_SAMPLE_INVERSE returns the inverse of a sample Wishart matrix.
!
!  Discussion:
!
!    This function requires functions from the PDFLIB and RNGLIB libraries.
!
!    The "initialize()" function from RNGLIB must be called before using
!    this function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Patrick Odell, Alan Feiveson,
!    A numerical procedure to generate a sample covariance matrix,
!    Journal of the American Statistical Association,
!    Volume 61, Number 313, March 1966, pages 199-203.
!
!    Stanley Sawyer,
!    Wishart Distributions and Inverse-Wishart Sampling,
!    Washington University,
!    30 April 2007, 12 pages.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the matrix.
!
!    Input, integer ( kind = 4 ) DF, the number of degrees of freedom.
!    M <= DF.
!
!    Input, real ( kind = 8 ) SIGMA(M,M), the covariance matrix, which should be 
!    a symmetric positive definite matrix.
!
!    Output, real ( kind = 8 ) A(M,M), the inverse of a sample matrix from the 
!    Wishart distribution.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a(m,m)
  integer ( kind = 4 ) df
  integer ( kind = 4 ) flag
  real ( kind = 8 ), allocatable :: r(:,:)
  real ( kind = 8 ), allocatable :: s(:,:)
  real ( kind = 8 ) sigma(m,m)
  real ( kind = 8 ), allocatable :: ua(:,:)

  if ( df < m ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WISHART_SAMPLE_INVERSE - Fatal error!'
    write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
    stop 1
  end if
!
!  Get R, the upper triangular Cholesky factor of SIGMA.
!
  allocate ( r(1:m,1:m) )
  call r8mat_cholesky_factor_upper ( m, sigma, r, flag )

  if ( flag /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'WISHART_SAMPLE_INVERSE - Fatal error!'
    write ( *, '(a)' ) &
      '  Unexpected error return from R8MAT_CHOLESKY_FACTOR_UPPER.'
    write ( *, '(a,i4)' ) '  FLAG = ', flag
    stop 1
  end if
!
!  Get S, the inverse of R.
!
  allocate ( s(1:m,1:m) )
  s(1:m,1:m) = r(1:m,1:m)
  call r8ut_inverse ( m, s )
!
!  Get UA, the inverse of a sample from the unit Wishart distribution.
!
  allocate ( ua(1:m,1:m) )
  call wishart_unit_sample_inverse ( m, df, ua )
!
!  Construct the matrix A = S * UA * S'.
!
  a = matmul ( s, matmul ( ua, transpose ( s ) ) )
!
!  Free memory.
!
  deallocate ( r )
  deallocate ( s )
  deallocate ( ua )

  return
end
subroutine wishart_unit_sample ( m, df, a )

!*****************************************************************************80
!
!! WISHART_UNIT_SAMPLE samples the unit Wishart distribution.
!
!  Discussion:
!
!    This function requires functions from the PDFLIB and RNGLIB libraries.
!
!    The "initialize()" function from RNGLIB must be called before using
!    this function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Patrick Odell, Alan Feiveson,
!    A numerical procedure to generate a sample covariance matrix,
!    Journal of the American Statistical Association,
!    Volume 61, Number 313, March 1966, pages 199-203.
!
!    Stanley Sawyer,
!    Wishart Distributions and Inverse-Wishart Sampling,
!    Washington University,
!    30 April 2007, 12 pages.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the matrix.
!
!    Input, integer ( kind = 4 ) DF, the number of degrees of freedom.
!    M <= DF.
!
!    Output, double A(M,M), the sample matrix from the 
!    unit Wishart distribution.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a(m,m)
  real ( kind = 8 ), allocatable :: c(:,:)
  integer ( kind = 4 ) df
  real ( kind = 8 ) df_chi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_chi_sample
  real ( kind = 8 ) r8_normal_01_sample

  if ( df < m ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WISHART_UNIT_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
    stop 1
  end if

  allocate ( c(1:m,1:m) )

  do i = 1, m

    do j = 1, i - 1
      c(i,j) = 0.0D+00
    end do

    df_chi = real ( df + 1 - i, kind = 8 )
    c(i,i) = sqrt ( r8_chi_sample ( df_chi ) )

    do j = i + 1, m
      c(i,j) = r8_normal_01_sample ( )
    end do

  end do

  a = matmul ( transpose ( c ), c )
!
!  Free memory.
!
  deallocate ( c )

  return
end
subroutine wishart_unit_sample_inverse ( m, df, a )

!*****************************************************************************80
!
!! WISHART_UNIT_SAMPLE_INVERSE inverts a unit Wishart sample matrix.
!
!  Discussion:
!
!    This function requires functions from the PDFLIB and RNGLIB libraries.
!
!    The "initialize()" function from RNGLIB must be called before using
!    this function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Patrick Odell, Alan Feiveson,
!    A numerical procedure to generate a sample covariance matrix,
!    Journal of the American Statistical Association,
!    Volume 61, Number 313, March 1966, pages 199-203.
!
!    Stanley Sawyer,
!    Wishart Distributions and Inverse-Wishart Sampling,
!    Washington University,
!    30 April 2007, 12 pages.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the matrix.
!
!    Input, integer ( kind = 4 ) DF, the number of degrees of freedom.
!    M <= DF.
!
!    Output, real ( kind = 8 ) A(M,M), the inverse of a sample matrix from 
!    the unit Wishart distribution.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a(m,m)
  real ( kind = 8 ), allocatable :: b(:,:)
  real ( kind = 8 ), allocatable :: c(:,:)
  integer ( kind = 4 ) df
  real ( kind = 8 ) df_chi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_chi_sample
  real ( kind = 8 ) r8_normal_01_sample

  if ( df < m ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WISHART_UNIT_SAMPLE_INVERSE - Fatal error!'
    write ( *, '(a,i6,a,i6)' ) '  DF = ', df, ' < M = ', m
    stop 1
  end if
!
!  Compute C, an upper triangular matrix such that the
!  Wishart sample matrix is C' * C.
!
  allocate ( c(1:m,1:m) )

  do i = 1, m
    df_chi = real ( df - i + 1, kind = 8 )
    c(i,i) = sqrt ( r8_chi_sample ( df_chi ) )
    do j = i + 1, m
      c(i,j) = r8_normal_01_sample ( )
    end do
  end do
!
!  Compute B, the inverse of C.
!
  allocate ( b(1:m,1:m) )
  b(1:m,1:m) = c(1:m,1:m)
  call r8ut_inverse ( m, b )
!
!  The inverse of the Wishart sample matrix C'*C is inv(C) * C'.
!
  a = matmul ( b, transpose ( b ) )
!
!  Free memory.
!
  deallocate ( b )
  deallocate ( c )

  return
end
