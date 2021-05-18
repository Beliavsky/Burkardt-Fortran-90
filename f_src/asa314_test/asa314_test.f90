program main

!*****************************************************************************80
!
!! MAIN is the main program for ASA314_TEST.
!
!  Discussion:
!
!    ASA314_TEST tests the ASA314 library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Roger Payne,
!    Inversion of matrices with contents subject to modulo arithmetic,
!    Applied Statistics,
!    Volume 46, Number 2, 1997, pages 295-298.
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ASA314_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ASA314 library.'

  call test01 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ASA314_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests INVMOD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Roger Payne,
!    Inversion of matrices with contents subject to modulo arithmetic,
!    Applied Statistics,
!    Volume 46, Number 2, 1997, pages 295-298.
!
  implicit none

  integer ( kind = 4 ), parameter :: nrow = 3

  integer ( kind = 4 ) cmod(nrow)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifault
  integer ( kind = 4 ) imat(nrow,nrow)
  integer ( kind = 4 ), dimension(nrow,nrow) :: jmat = reshape ( (/ &
    1, 0, 0, 2, 1, 0, 1, 0, 1 /), (/ nrow, nrow /) )
  integer ( kind = 4 ), dimension(nrow,nrow) :: mat = reshape ( (/ &
    1, 0, 0, 1, 1, 0, 2, 0, 1 /), (/ nrow, nrow /) )
  integer ( kind = 4 ) rmod(nrow)

  do i = 1, nrow
    cmod(i) = 3
    rmod(i) = 3
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  INVMOD computes the inverse of a matrix'
  write ( *, '(a)' ) '  whose elements are subject to modulo arithmetic.'

  call i4mat_print ( nrow, nrow, mat, '  The matrix to be inverted:' )

  call invmod ( mat, imat, rmod, cmod, nrow, ifault )

  call i4mat_print ( nrow, nrow, imat, '  The computed inverse:' )

  call i4mat_print ( nrow, nrow, jmat, '  The correct inverse:' )

  return
end

