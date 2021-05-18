program main

!*****************************************************************************80
!
!! MAIN is the main program for MONOMIAL_VALUE_TEST.
!
!  Discussion:
!
!    MONOMIAL_VALUE_TEST tests the MONOMIAL_VALUE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MONOMIAL_VALUE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the MONOMIAL_VALUE library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MONOMIAL_VALUE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests MONOMIAL_VALUE on sets of data in various dimensions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ), allocatable :: e(:)
  integer ( kind = 4 ) e_max
  integer ( kind = 4 ) e_min
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: v(:)
  real ( kind = 8 ), allocatable :: x(:,:)
  real ( kind = 8 ) x_max
  real ( kind = 8 ) x_min

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Usine monomial_value to evaluate monomials in'
  write ( *, '(a)' ) '  dimensions 1 through 3.' 

  e_min = -3
  e_max = 6
  n = 5
  seed = 123456789
  x_min = -2.0D+00
  x_max = +10.0D+00

  do m = 1, 3

    write ( *, '(a)' ) ''
    write ( *, '(a,i1)' ) '  Spatial dimension M = ', m

    allocate ( e(1:m) )
    allocate ( x(1:m,1:n) )
    allocate ( v(1:n) )

    call i4vec_uniform_ab ( m, e_min, e_max, seed, e )
    call i4vec_transpose_print ( m, e, '  Exponents:' )
    call r8mat_uniform_ab ( m, n, x_min, x_max, seed, x )
!
!  To make checking easier, make the X values integers.
!
    call r8mat_nint ( m, n, x )
    call monomial_value ( m, n, e, x, v )

    write ( *, '(a)' ) ''
    write ( *, '(a)', advance = 'no' ) '   V(X)         '
    do i = 1, m
      write ( *, '(a,i1,a)', advance = 'no' ) '      X(', i, ')'
    end do
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) ''
    do j = 1, n
      write ( *, '(g14.6,2x,3f10.4)' ) v(j), x(1:m,j)
    end do

    deallocate ( e )
    deallocate ( x )
    deallocate ( v )

  end do

  return
end
