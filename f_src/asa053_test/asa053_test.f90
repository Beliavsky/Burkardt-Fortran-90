program main

!*****************************************************************************80
!
!! MAIN is the main program for ASA053_TEST.
!
!  Discussion:
!
!    ASA053_TEST tests ASA053.
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
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ASA053_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ASA053 library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ASA053_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 generates a random Wishart variate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 April 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: np = 3

  real ( kind = 8 ) :: d((np*(np+1))/2) = (/ &
    3.0D+00, &
    2.0D+00, 4.0D+00, &
    1.0D+00, 2.0D+00, 5.0D+00 /)
  integer ( kind = 4 ) n
  real ( kind = 8 ) sa((np*(np+1))/2)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Generate a single Wishart deviate.'

  n = 1
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The number of variables is ', np
  write ( *, '(a,i4)' ) '  The number of degrees of freedom is ', n

  call r8utp_print ( np, d, '  The upper Cholesky factor:' )

  call wshrt ( d, n, np, seed, sa )

  call r8pp_print ( np, sa, '  The sample matrix:' )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 averages many Wishart samples.
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
  implicit none

  integer ( kind = 4 ), parameter :: np = 3

  integer ( kind = 4 ), parameter :: npp = ( np * ( np + 1 ) ) / 2

  real ( kind = 8 ) :: d(npp) = (/ &
    3.0D+00, &
    2.0D+00, 4.0D+00, &
    1.0D+00, 2.0D+00, 5.0D+00 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) ki
  integer ( kind = 4 ) kj
  integer ( kind = 4 ) n
  real ( kind = 8 ) s_average(npp)
  real ( kind = 8 ) sa(npp)
  real ( kind = 8 ) sigma(np,np)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Average many Wishart samples.'
  write ( *, '(a)' ) '  Compare to D'' * D * np / n.'

  n = 2
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The number of variables is ', np
  write ( *, '(a,i4)' ) '  The number of degrees of freedom is ', n

  call r8utp_print ( np, d, '  The upper Cholesky factor:' )

  s_average(1:npp) = 0.0D+00

  test_num = 100000
  do i = 1, test_num
    call wshrt ( d, n, np, seed, sa )
    s_average(1:npp) = s_average(1:npp) + sa(1:npp)
  end do

  s_average(1:npp) = s_average(1:npp) / dble ( test_num )

  call r8pp_print ( np, s_average, '  The averaged matrix:' )
!
!  Compare the result to ( D' * D ) * np / n.
!
  sigma(1:np,1:np) = 0.0D+00
  
  do i = 1, np
    do j = 1, np
      do k = 1, min ( i, j )
        ki = k + ( i * ( i - 1 ) ) / 2
        kj = k + ( j * ( j - 1 ) ) / 2
        sigma(i,j) = sigma(i,j) + d(ki) * d(kj)
      end do
      sigma(i,j) = sigma(i,j) * dble ( np ) / dble ( n )
    end do
  end do

  call r8mat_print ( np, np, sigma, '  Expected result:' )

  return
end

