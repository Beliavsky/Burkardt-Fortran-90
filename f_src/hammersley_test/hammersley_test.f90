program main

!*****************************************************************************80
!
!! MAIN is the main program for HAMMERSLEY_TEST.
!
!  Discussion:
!
!    HAMMERSLEY_TEST tests the HAMMERSLEY library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HAMMERSLEY_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HAMMERSLEY library'

  call hammersley_test ( )
  call hammersley_sequence_test ( )
  call hammersley_inverse_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HAMMERSLEY_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine hammersley_test ( )

!*****************************************************************************80
!
!! HAMMERSLEY_TEST tests HAMMERSLEY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) r(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HAMMERSLEY_TEST'
  write ( *, '(a)' ) '  HAMMERSLEY returns the I-th element of an M-dimensional'
  write ( *, '(a)' ) '  Hammersley sequence.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I          HAMMERSLEY(I)'

  n = 16

  do m = 1, 3
    write ( *, '(a)' ) ''
    write ( *, '(a,i2)' ) '  Use M = ', m
    write ( *, '(a,i2)' ) '      N = ', n
    write ( *, '(a)' ) ''
    do i = 0, 10
      call  hammersley ( i, m, n, r )
      write ( *, '(2x,i3,3(2x,f14.8))' ) i, r(1:m)
    end do
  end do

  return
end
subroutine hammersley_inverse_test ( )

!*****************************************************************************80
!
!! HAMMERSLEY_INVERSE_TEST tests HAMMERSLEY_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) r(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HAMMERSLEY_INVERSE_TEST'
  write ( *, '(a)' ) '  HAMMERSLEY_INVERSE inverts an element of a Hammersley sequence.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I        R=HAMMERSLEY(I,3)  HAMMERSLEY_INVERSE(R,3)'
  write ( *, '(a)' ) ''

  m = 3
  n = 16

  do i = 0, 10
    call hammersley ( i, m, n, r )
    call hammersley_inverse ( r, m, n, i2 )
    write ( *, '(2x,i3,3(2x,f14.8),2x,i3)' ) i, r(1:m), i2
  end do

  return
end
subroutine hammersley_sequence_test ( )

!*****************************************************************************80
!
!! HAMMERSLEY_SEQUENCE_TEST tests HAMMERSLEY_SEQUENCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) r(3,11)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HAMMERSLEY_SEQUENCE_TEST'
  write ( *, '(a)' ) '  HAMMERSLEY_SEQUENCE returns the elements I1 through I2'
  write ( *, '(a)' ) '  of an M-dimensional Hammersley sequence.'

  n = 16

  do m = 1, 3
    write ( *, '(a)' ) ''
    write ( *, '(a,i1,a)' ) '  HAMMERSLEY_SEQUENCE(0,10,', m, 'N,R):'
    call hammersley_sequence ( 0, 10, m, n, r )
    call r8mat_print ( m, 11, r, '  R:' )
  end do

  m = 3
  n = 16
  write ( *, '(a)' ) ''
  write ( *, '(a,i1,a)' ) '  HAMMERSLEY_SEQUENCE(10,0,', m, ',N,R):'
  call hammersley_sequence ( 10, 0, m, n, r )
  call r8mat_print ( m, 11, r, '  R:' )

  return
end

