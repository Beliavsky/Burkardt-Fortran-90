program main

!*****************************************************************************80
!
!! MAIN is the main program for PROBLEM1_MAIN.
!
!  Discussion:
!
!    The coding of PROBLEM1 is tricky enough that I want to be able to
!    try it out independently of the DREAM code.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) chain_num
  integer ( kind = 4 ) cr_num
  integer ( kind = 4 ) gen_num
  integer ( kind = 4 ) pair_num
  integer ( kind = 4 ) par_num
  integer ( kind = 4 ) sample_num

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PROBLEM1_MAIN'
  write ( *, '(a)' ) '  FORTRAN90 version'
!
!  Initialize the random number generator library.
!
  call initialize ( )
!
!  By calling PROBLEM_SIZE, we implicitly set up the covariance as well.
!
  call problem_size ( chain_num, cr_num, gen_num, pair_num, par_num )

  sample_num = 10000
  call test01 ( par_num, sample_num )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PROBLEM1_MAIN'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine test01 ( par_num, sample_num )

!*****************************************************************************80
!
!! TEST01 calls the sampling function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2013
!
!  Author:
!
!    John Burkardt
!
  use covariance

  implicit none

  integer ( kind = 4 ) par_num
  integer ( kind = 4 ) sample_num

  real ( kind = 8 ) cov_sample(par_num,par_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) zp(par_num,sample_num)
  real ( kind = 8 ) zp_ave(par_num)
  real ( kind = 8 ) zp_max(par_num)
  real ( kind = 8 ) zp_min(par_num)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Call PRIOR_SAMPLE many times.'
  write ( *, '(a)' ) '  Compare statistics to PDF parameters.'
  write ( *, '(a)' ) '  Note that the covariance estimate can be very bad'
  write ( *, '(a)' ) '  unless the matrix is strongly diagonal.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  Parameter dimension is ', par_num
  write ( *, '(a,i6)' ) '  Number of samples is ', sample_num
!
!  Compute N multinormal samples.
!
  do j = 1, sample_num
    call prior_sample ( par_num, zp(1:par_num,j) )
  end do

  do i = 1, par_num
    zp_min(i) = minval ( zp(i,1:sample_num) )
    zp_max(i) = maxval ( zp(i,1:sample_num) )
    zp_ave(i) = sum ( zp(i,1:sample_num) ) / real ( sample_num, kind = 8 )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' Index       Min            Ave              Max             MU'
  write ( *, '(a)' ) ' '
  do i = 1, par_num
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, zp_min(i), zp_ave(i), zp_max(i), zp_mean(i)
  end do

  call r8mat_covariance ( par_num, sample_num, zp, cov_sample )

  call r8mat_print ( par_num, par_num, cov_sample, '  Sample covariance:' )

  call r8mat_print ( par_num, par_num, c, '  PDF covariance:' )

  return
end
subroutine r8mat_covariance ( m, n, x, c )

!*****************************************************************************80
!
!! R8MAT_COVARIANCE computes the sample covariance of a set of vector data.
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
!    26 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the size of a single data vectors.
!
!    Input, integer ( kind = 4 ) N, the number of data vectors.
!    N should be greater than 1.
!
!    Input, real ( kind = 8 ) X(M,N), an array of N data vectors, each
!    of length M.
!
!    Output, real ( kind = 8 ) C(M,M), the covariance matrix for the data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) c(m,m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) x_mean(m)

  c(1:m,1:m) = 0.0D+00
!
!  Special case of N = 1.
!
  if ( n == 1 ) then
    do i = 1, m
      c(i,i) = 1.0D+00
    end do
    return
  end if
!
!  Determine the sample means.
!
  do i = 1, m
    x_mean(i) = sum ( x(i,1:n) ) / real ( n, kind = 8 )
  end do
!
!  Determine the sample covariance.
!
  do j = 1, m
    do i = 1, m
      do k = 1, n
        c(i,j) = c(i,j) + ( x(i,k) - x_mean(i) ) * ( x(j,k) - x_mean(j) )
      end do
    end do
  end do

  c(1:m,1:m) = c(1:m,1:m) / real ( n - 1, kind = 8 )

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
